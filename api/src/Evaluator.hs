{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE GADTs #-}

module Evaluator
  ( apply
  , lookupCard
  )
  where

import           Control.Lens         (Lens', at, ix, non, over, preview, set,
                                       view)
import           Control.Monad        (foldM)
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.Writer (tell)
import           Data.Foldable        (find)
import           Data.Maybe           (catMaybes, fromJust)
import           Data.Sequence        (Seq ((:<|), Empty), (<|), (|>))
import qualified Data.Sequence        as S
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

import Debug.Trace

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

import CardLang
import CardLang.Evaluator (fromU, toU, showCode)

logAction :: Action -> GameMonad ()
logAction a = tell (S.singleton a)

-- Applies an action to the current board, returning the resulting one.
apply :: Action -> GameMonad Board
apply (ActionOptional a ifyes ifno) = do
  pid <- currentPlayer
  choices <- view (playerChoices . at pid . non mempty) <$> currentBoard

  case choices of
    ChoosePass :<| _ -> apply ifno
    _                -> do
      board' <- apply a `catchError` handler
      withBoard board' $ apply ifyes

  where
    handler (board, action) = throwError (board, ActionOptional action ifyes ifno)

apply (ActionAllowFail a) = do
  board' <- apply a `catchError` handler

  if isLost board' then
    currentBoard
  else
    return board'
  where
    handler (board, action) = do
      board' <- currentBoard
      if isLost board then
        return board'
      else
        throwError (board, ActionAllowFail action)

apply a@(ActionMove specificCard@(location, i) to dest) = do
  maybeCard <- lookupCard specificCard

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a specificCard
    Just card -> do
                   board <- currentBoard

                   logAction (ActionMove specificCard to dest)

                   return $
                       over (cardsAtLocation location) (S.deleteAt i)
                     . over (cardsAtLocation to) ((insertF dest) card)
                     . clearChoices -- TODO: This is a bit weird. See notes at query TChooseCard
                     $ board

  where
    insertF Back = flip (|>)
    insertF Front = (<|)
    insertF (LocationIndex i) = S.insertAt i
apply a@(ActionReveal location) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> do
      board <- overCard location (setVisibility All)

      --logAction a

      return board
apply a@(ActionHide location) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> do
      board <- overCard location (setVisibility Hidden)

      --logAction a

      return board

apply (ActionRecruit pid amount) = do
  apply (ApplyResources pid $ set money amount mempty)

apply (ActionAttack pid amount) = do
  apply (ApplyResources pid $ set attack amount mempty)
-- Implemented as a separate action so that we don't lose semantic meaning of "KO"
apply (ActionKO location) = apply $ ActionMove location KO Front
apply (ActionDiscardCard location) = do
  pid <- owner location
  apply $ ActionMove location (PlayerLocation pid Discard) Front

  where
    owner (PlayerLocation pid _, _) = return pid
    owner _ = lose "Cannot discard a card not in a player location"

apply (ActionDraw pid amount) = apply -- TODO: Respect amount
     ((ActionReveal (PlayerLocation pid PlayerDeck, 0))
  <> (ActionMove
        (PlayerLocation pid PlayerDeck, 0)
        (PlayerLocation pid Hand)
        Front))

apply (ActionRescueBystander pid 0) = apply ActionNone
apply (ActionRescueBystander pid n) = do
  apply $ ActionMove
            (BystanderDeck, 0)
            (PlayerLocation pid Victory)
            Front
       <> ActionRescueBystander pid (n - 1)

apply a@(ApplyResources (PlayerId id) rs) = do
  board <- currentBoard

  logAction a

  let board' = over (players . ix id . resources) (rs <>) board

  if invalidResources (view (players . ix id . resources) board') then
    lose "Not enough resources"
  else
    return board'

apply ActionNone = currentBoard

apply (ActionCombine a b) = do
  board' <- apply a `catchError` handler

  withBoard board' $ apply b

  where
    handler (board, action) = throwError (board, ActionCombine action b)

apply a@(ActionTrace _) = do
  logAction a
  currentBoard

apply a@(ActionShuffle location) = do
  board <- currentBoard

  let g = view rng board
  let cs = view (cardsAtLocation location) board
  let (shuffled, g') = shuffleSeq g cs

  logAction a

  return
    . set
        (cardsAtLocation location)
        shuffled
    . set rng g'
    $ board

apply ActionPrepareGame = do
  tag "Prepare game" $ do
    preparePlayers <-   mconcat . toList
                      . fmap (preparePlayer . view playerId)
                      . view players
                      <$> currentBoard

    apply $
               preparePlayers
            <> (mconcat $ fmap ActionShuffle [HeroDeck, VillainDeck])
            <> (mconcat $ fmap replaceHeroInHQ [0..4])
            <> ActionStartTurn

  where
    preparePlayer pid =    ActionShuffle (PlayerLocation pid PlayerDeck)
                        <> drawAction 6 pid

apply ActionEndTurn = do
  player <- currentPlayer

  tag (playerDesc player <> " turn end") $ do
    board  <-   set
                  (playerResources player)
                  mempty
              . moveAllFrom
                  (cardsAtLocation $ PlayerLocation player Played)
                  (cardsAtLocation $ PlayerLocation player Discard)
              . moveAllFrom
                  (cardsAtLocation $ PlayerLocation player Hand)
                  (cardsAtLocation $ PlayerLocation player Discard)
              <$> currentBoard

    withBoard board $
      over players moveHeadToTail <$> apply (drawAction 6 player)

apply ActionStartTurn = do
  pid <- currentPlayer

  tag (playerDesc pid <> " turn start") $ do
    let topCard = (VillainDeck, 0)
    nextCard <- lookupCard topCard

    case view cardTemplate <$> nextCard of
      Nothing -> lose "Draw! No more villians."
      Just BystanderCard -> do
        board <- currentBoard
        let nearestVillain = find
                               (villainAtLocation board)
                               allCityLocations

        let action = case nearestVillain of
                       Just location -> revealAndMove topCard location Back

                       -- TODO: Move to mastermind
                       Nothing       -> revealAndMove topCard KO Front

        apply $ action <> ActionPlayerTurn pid

      Just EnemyCard{} -> do
        (board, effect) <- moveCity (City 0) mempty

        withBoard board . apply $
             effect
          <> revealAndMove (VillainDeck, 0) (City 0) Front
          <> ActionPlayerTurn pid
  where
    villainAtLocation board l =
      not . null . view (cardsAtLocation l) $ board

apply (ActionTagged reason action) = tag reason (apply action)

apply a@(ActionChooseCard desc options expr pass) = applyChoices f
  where
    f (ChoosePass :<| _) = return pass
    f (ChooseCard location :<| _) = do
      if elem location options then
        do
          board <- currentBoard
          case fromU $ evalWith (mkEnv $ Just board) (UApp expr (UConst $ toU location)) of
             Right x -> return x
             Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y

      else
        return mempty
    f _ = wait a desc


apply a@(ActionPlayerTurn _) = applyChoices f
  where
    clearAndApply action = do
      pid <- currentPlayer
      board' <- clearChoices <$> currentBoard

      withBoard board' . apply $ action

    f :: S.Seq PlayerChoice -> GameMonad Action
    f (ChooseCard location@(PlayerLocation pid' Hand, i) :<| _) = do
      pid <- currentPlayer

      if pid == pid' then
        do
          board <- currentBoard
          card       <- requireCard location

          let cardCode = fromJust $ preview (cardTemplate . playCode) card

          let ret = evalWith (mkEnv $ Just board) cardCode

          action <- case fromU $ ret of
                      Right x -> return x
                      Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y


          let continue = revealAndMove location (PlayerLocation pid Played) Front
                      <> action
          let guardCode = fromJust $ preview (cardTemplate . playGuard) card

          let ret = evalWith (mkEnv $ Just board) (UApp guardCode (UConst . toU $ continue))

          case fromU ret of
            Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y
            Right x -> return . ActionTagged (playerDesc pid <> " plays " <> view (cardTemplate . cardName) card) $
                         x
                      <> a
      else
        f mempty

    f (ChooseCard location@(HQ, i) :<| _) = do
      card <- requireCard location
      pid <- currentPlayer

      let template = view cardTemplate card

      return . ActionTagged (playerDesc pid <> " purchases " <> view cardName template) $
           (ActionMove location (PlayerLocation pid Discard) Front)
        <> (ActionRecruit pid (-(view heroCost template)))
        <> replaceHeroInHQ i

    f (ChooseCard location@(City n, i) :<| _) = do
      card <- requireCard location
      pid <- currentPlayer

      let template = view cardTemplate card

      return . ActionTagged (playerDesc pid <> " attacks " <> view cardName template) $
           ActionMove location (PlayerLocation pid Victory) Front
        <> ActionAttack pid (negate . view baseHealth $ template)

    f (ChooseEndTurn :<| _) = do
      pid <- currentPlayer

      return $
           ActionEndTurn
        <> ActionStartTurn
    f _ = do
      pid <- currentPlayer

      wait a $ playerDesc pid <> "'s turn"

apply a@(ActionLose reason) = lose reason
apply (ActionHalt a reason) = wait a reason

apply (ActionConcurrent as) = do
  -- Each action will return a board, and optionally halt. If it halts, keep
  -- it in the list of actions and halt with ActionConcurrent.
  board <- currentBoard

  (board, haltInfo) <- foldM f (board, []) as

  if null haltInfo then
    -- A player may have set choices without having a blocking action, we don't
    -- want those choices to persist through to whatever the next action might
    -- be. Hence, clear them. For any feature like allowing for play choices
    -- before their turn, most likely want to implement that on the client.
    -- It's unknown whether any effect will interrupt that would overload those
    -- choices, and we don't want a player discarding something they had
    -- intended to play.  (Another option would be to scope choices to a phase,
    -- but that's not a concept we've reified right now.)
    return $ clearChoices board
  else
    let (as', messages) = unzip . reverse $ haltInfo in
    throwError (set boardState (mconcat messages) board, ActionConcurrent as')

  where
    f (board, as) a = do
      withBoard board (do
        board' <- apply a

        return (board', as)) `catchError` handler

      where
        handler (b, e) = return (b, (e, view boardState b):as)

apply a@(ActionDiscard pid) = applyChoicesFor pid discardSelection
  where
    discardSelection (ChooseCard location@(PlayerLocation pid' Hand, i) :<| _)
      | pid == pid' = do
        card <- requireCard location

        return $ ActionMove
                   location
                   (PlayerLocation pid Discard)
                   Front
    discardSelection _ = return . ActionHalt a $
      playerDesc pid <> ": select a card in hand to discard"

apply ActionKOHero = applyChoices koHero
  where
    koHero (ChooseCard location@(HQ, i) :<| _) = do
      pid   <- currentPlayer
      valid <- checkCondition (ConditionCostLTE location 6)

      return $ if valid then
             ActionMove location KO Front
          <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)
      else
        ActionHalt ActionKOHero $
          playerDesc pid <> ": select a card in HQ costing 6 or less to KO"
    koHero _ = do
      pid <- currentPlayer

      return . ActionHalt ActionKOHero $
        playerDesc pid <> ": select a card in HQ costing 6 or less to KO"

apply a = lose $ "Unknown action: " <> showT a

moveCity :: Location -> S.Seq CardInPlay -> GameMonad (Board, Action)
moveCity Escaped incoming =
  case incoming of
    (card :<| other) -> case view cardTemplate card of
                          EnemyCard{} -> do
                            board <- currentBoard
                            pid <- currentPlayer

                            let discardAction = ActionConcurrent . toList $
                                                  fmap (ActionDiscard . view playerId) (view players board)

                            return $ (board, ActionKOHero <> discardAction)
                          _ -> loseWithEffect "Unhandled incoming in Escaped handler"
    _                -> loseWithEffect "No incoming cards in Escaped handler"

  where
    loseWithEffect reason = do
      board <- lose reason

      return (board, ActionNone)

moveCity location@(City i) incoming = do
  cardsHere <- view (cardsAtLocation location) <$> currentBoard

  let recurse = not . S.null $ cardsHere
  let nextLocation = nextL location

  (board, effect) <- if recurse then
                       moveCity nextLocation cardsHere
                     else
                       do
                         board <- currentBoard

                         return (board, ActionNone)

  return $ (moveAllFrom
             (cardsAtLocation location)
             (cardsAtLocation nextLocation)
             board, effect)

  where
    nextL (City 4) = Escaped
    nextL (City i) = City $ i + 1

moveAllFrom :: Lens' Board (S.Seq CardInPlay)
            -> Lens' Board (S.Seq CardInPlay)
            -> Board
            -> Board
moveAllFrom src dest board =
  let cs = view src board in

  set src mempty . over dest (cs <>) $ board

tryShuffleDiscardToDeck :: Action -> SpecificCard -> GameMonad Board
tryShuffleDiscardToDeck a specificCard = do
  board <- currentBoard

  let (location, _) = specificCard
  location <- resolveLocation location

  case playerDeck location of
    Nothing -> lose $ "Card does not exist: " <> showT specificCard
    Just playerId -> do
      let discardDeck = PlayerLocation playerId Discard

      case view (cards . at discardDeck . non mempty) board of
        Empty -> lose $ "No cards left to draw for " <> showT playerId
        cs -> apply $
                (ActionTagged
                  (playerDesc playerId <> " shuffles discard into deck") $
                    moveAndHide (length cs) discardDeck location
                  <> ActionShuffle location
                )
                <> a

  where
    -- Returns the relevant PlayerId if the given location is a player's deck.
    -- This is oddly specific so there's probably a better way to structure the
    -- above code.
    playerDeck :: Location -> Maybe PlayerId
    playerDeck (PlayerLocation playerId PlayerDeck) = Just playerId
    playerDeck _ = Nothing

    moveAndHide :: Int -> Location -> Location -> Action
    moveAndHide n from to =
      mconcat . toList . replicate n $
           ActionMove (from, 0) to Front
        <> ActionHide (to, 0)

overCard :: SpecificCard -> (CardInPlay -> CardInPlay) -> GameMonad Board
overCard (location, i) f = do
  location <- resolveLocation location

  over (cardsAtLocation location . ix i) f <$> currentBoard

halt a = do
  b <- currentBoard

  throwError (b, a)

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility v = set cardVisibility v

invalidResources :: Resources -> Bool
invalidResources r = (view money r < mempty) || (view attack r < mempty)


wait :: Action -> T.Text -> GameMonad a
wait a reason = do
  b <- currentBoard

  throwError (set boardState (WaitingForChoice reason) b, a)

resolveLocation :: Location -> GameMonad Location
resolveLocation (PlayerLocation CurrentPlayer x) = do
  pid <- currentPlayer

  return $ PlayerLocation pid x
resolveLocation x = return x

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) = do
  location <- resolveLocation location
  preview (cardsAtLocation location . ix i) <$> currentBoard

requireCard :: SpecificCard -> GameMonad CardInPlay
requireCard location = do
  b <- currentBoard
  maybeCard <- lookupCard location

  case maybeCard of
    Just c -> return c
    Nothing -> lose "No card at location"

currentPlayerChoices = do
  pid <- currentPlayer

  view (playerChoices . at pid . non mempty) <$> currentBoard

clearChoices =
  set playerChoices mempty

clearChoicesFor pid =
  set (playerChoices . at pid) mempty

applyChoices f = do
  pid <- currentPlayer

  applyChoicesFor pid f

applyChoicesFor pid f = do
  choices <- view (playerChoices . at pid . non mempty) <$> currentBoard

  a' <- f choices
  clearAndApply a'

  where
    clearAndApply action = do
      board' <- clearChoicesFor pid <$> currentBoard

      withBoard board' . apply $ action

checkCondition :: Condition -> GameMonad Bool
checkCondition (ConditionCostLTE location amount) = do
  card <- requireCard location

  return $ view (cardTemplate . heroCost) card <= (Sum amount)

tag :: T.Text -> GameMonad Board -> GameMonad Board
tag message m = do
  board <- currentBoard
  let (board', actions) = runGameMonad' board m

  case mconcat . toList $ actions of
    ActionNone   -> return ()
    foldedAction -> logAction (ActionTagged message foldedAction)

  return board'
