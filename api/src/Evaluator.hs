{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

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
import           Data.Maybe           (catMaybes)
import           Data.Sequence        (Seq ((:<|), Empty), (<|), (|>))
import qualified Data.Sequence        as S
import qualified Data.Text            as T

import Debug.Trace

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

logAction :: Action -> GameMonad ()
logAction a = tell (S.singleton a)

-- Action handlers
--
-- These are moved into their own functions for ease of profiling/debugging.
-- ==============

moveCard :: Action -> GameMonad Board
moveCard a@(MoveCard specificCard@(location, i) to dest) = do
  location <- resolveLocation location
  to <- resolveLocation to

  maybeCard <- lookupCard specificCard

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a specificCard
    Just card -> do
                   board <- currentBoard

                   logAction a

                   return $
                       over (cardsAtLocation location) (S.deleteAt i)
                     . over (cardsAtLocation to) ((insertF dest) card)
                     $ board

  where
    insertF Back = flip (|>)
    insertF Front = (<|)
    insertF (LocationIndex i) = S.insertAt i

revealCard a@(RevealCard location v) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> do
      board <- overCard location (setVisibility v)

      logAction a

      return board

queryPlayer :: QueryPlayer -> GameMonad PlayerId
queryPlayer QueryCurrentPlayer = currentPlayer

queryInt :: QueryInt -> GameMonad Int
queryInt (QueryConst x) = return x

-- Applies an action to the current board, returning the resulting one.
apply :: Action -> GameMonad Board
apply a@MoveCard{} = moveCard a
apply a@RevealCard{} = revealCard a
apply (ActionMoney qp pi) = do
  pid <- queryPlayer qp
  amount <- Sum <$> queryInt pi

  apply (ApplyResources pid $ set money amount mempty)

apply a@(ApplyResources (PlayerId id) rs) = do
  board <- currentBoard

  logAction a

  let board' = over (players . ix id . resources) (rs <>) board

  if invalidResources (view (players . ix id . resources) board') then
    lose "Not enough resources"
  else
    return board'

apply ActionNone = currentBoard

apply (ActionIf cond a b) = do
  branch <- checkCondition cond

  apply $ if branch then a else b

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
          card       <- requireCard location
          let cardEffect = view (cardTemplate . playEffect) card

          return . ActionTagged (playerDesc pid <> " plays " <> view (cardTemplate . cardName) card) $
               revealAndMove location (PlayerLocation pid Played) Front
            <> cardEffect
      else
        f mempty

    f (ChooseCard location@(HQ, i) :<| _) = do
      card <- requireCard location
      pid <- currentPlayer

      let template = view cardTemplate card

      return . ActionTagged (playerDesc pid <> " purchases " <> view cardName template) $
           MoveCard location (PlayerLocation pid Discard) Front
        <> ApplyResources pid (mempty { _money = -(view heroCost template)})
        <> replaceHeroInHQ i

    f (ChooseCard location@(City n, i) :<| _) = do
      card <- requireCard location
      pid <- currentPlayer

      let template = view cardTemplate card

      return . ActionTagged (playerDesc pid <> " attacks " <> view cardName template) $
           MoveCard location (PlayerLocation pid Victory) Front
        <> ApplyResources pid
             (set attack (negate . view baseHealth $ template) mempty)

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

        return $ MoveCard location (PlayerLocation pid Discard) Front
    discardSelection _ = return . ActionHalt a $
      playerDesc pid <> ": select a card in hand to discard"

apply ActionKOHero = applyChoices koHero
  where
    koHero (ChooseCard location@(HQ, i) :<| _) = do
      pid   <- currentPlayer
      valid <- checkCondition (ConditionCostLTE location 6)

      return $ if valid then
             MoveCard location KO Front
          <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)
      else
        ActionHalt ActionKOHero $
          playerDesc pid <> ": select a card in HQ costing 6 or less to KO"
    koHero _ = do
      pid <- currentPlayer

      return . ActionHalt ActionKOHero $
        playerDesc pid <> ": select a card in HQ costing 6 or less to KO"

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
           MoveCard (from, 0) to Front
        <> RevealCard (to, 0) Hidden

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
