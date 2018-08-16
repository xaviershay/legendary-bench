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
import           Control.Monad        (foldM, forM)
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.Writer (tell)
import           Data.Foldable        (find, asum)
import           Data.Maybe           (catMaybes, fromJust)
import           Data.Sequence        (Seq ((:<|), Empty), (<|), (|>))
import qualified Data.Sequence        as S
import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as M

import Debug.Trace

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

import CardLang
import CardLang.Evaluator (fromU, toU, toUConst, showCode)

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

apply a@(ActionMove specificCard to dest) = do
  let location = cardLocation specificCard
  i <- cardLocationIndex specificCard

  maybeCard <- lookupCard specificCard

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a specificCard
    Just card -> do
                   board <- currentBoard

                   logAction (ActionMove specificCard to dest)

                   return $
                       over (cardsAtLocation location) (S.deleteAt i)
                     . over (cardsAtLocation to) (insertF dest card)
                     . clearChoices -- TODO: This is a bit weird. See notes at query TChooseCard
                     $ board

  where
    insertF Back = flip (|>)
    insertF Front = (<|)
    insertF (LocationIndex i) = S.insertAt i
apply a@(ActionVisibility location vis) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> overCard location (setVisibility vis)

apply a@(ActionHide location) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> overCard location (setVisibility Hidden)

apply (ActionRecruit pid amount) =
  apply (ApplyResources pid $ set money amount mempty)

apply (ActionAttack pid amount) =
  apply (ApplyResources pid $ set attack amount mempty)
-- Implemented as a separate action so that we don't lose semantic meaning of "KO"
apply (ActionKO location) = apply $ revealAndMove location KO Front

apply (ActionDefeat pid address) = do
  let location = cardLocation address
  cs <- view (cardsAtLocation location) <$> currentBoard

  apply . mconcat $
    replicate
      (length cs)
      (ActionMove (cardByIndex location 0) (PlayerLocation pid Victory) Front)

apply (ActionDiscardCard location) = do
  pid <- owner location
  -- TODO: See comments against discard-hand LISP function as to why this is
  -- lookupCard rather than requireCard.
  card <- lookupCard location

  case card of
    Nothing -> currentBoard
    Just card -> do
      board <- apply $ ActionMove location (PlayerLocation pid Discard) Front

      withBoard board $ do
        let newLocation = cardById (PlayerLocation pid Discard) (view cardId card)
        let expr = fromJust $ preview (cardTemplate . discardEffect) card

        case fromU $ evalWith (mkEnv $ Just board) (UApp expr (toUConst newLocation)) of
          Left y -> lose $ "Unexpected state: board function doesn't evalute to an action. Got: " <> y
          Right action -> apply action

  where
    owner address = case cardLocation address of
                      PlayerLocation pid _ -> return pid
                      _ -> lose "Cannot discard a card not in a player location"

apply (ActionDraw pid (Sum n)) = apply $
  ActionTagged (playerDesc pid <> " draws " <> showT n) $
    mconcat . replicate n $
         ActionVisibility (cardByIndex (PlayerLocation pid PlayerDeck) 0) Owner
      <> ActionMove
            (cardByIndex (PlayerLocation pid PlayerDeck) 0)
            (PlayerLocation pid Hand)
            Front

-- TODO: Handle not having any bystanders left
apply (ActionRescueBystander _ 0) = apply mempty
apply (ActionRescueBystander pid n) =
  apply $ ActionMove
            (cardByIndex BystanderDeck 0)
            (PlayerLocation pid Victory)
            Front
       <> ActionRescueBystander pid (n - 1)

-- TODO: Handle not having any wounds left. Verify what happens for replacement
-- effects.
apply (ActionGainWound _ _ 0) = apply mempty
apply (ActionGainWound pid dest n) = do
  -- Find any cards from hand or played that might replace this effect
  board <- currentBoard
  let ls = concatMap
             (\loc -> map (cardById loc . view cardId) (filter isHero . toList . view (cardsAtLocation loc) $ board))
             [PlayerLocation pid Hand, PlayerLocation pid Played]

  let effect = ActionMove
                 (cardByIndex WoundDeck 0)
                 dest
                 Front

  a <- foldM effectReducer effect ls

  board' <- apply a

  withBoard board' . apply $ ActionGainWound pid dest (n - 1)

  where
    isHero x = case view cardTemplate x of
                 HeroCard{} -> True
                 _          -> False
    effectReducer a l = do
      c <- requireCard l
      board <- currentBoard

      let code = view (cardTemplate . woundEffect) c

      case fromU $ evalWith (mkEnv $ Just board) (UApp (UApp code (toUConst a)) (toUConst l)) of
        Left y -> lose $ "Unexpected state: expected action got " <> y
        Right replacement -> return replacement

apply (ActionCaptureBystander _ 0) = apply ActionNone
apply (ActionCaptureBystander sloc n) =
  apply $ ActionMove
            (cardByIndex BystanderDeck 0)
            (cardLocation sloc)
            Back
       <> ActionCaptureBystander sloc (n - 1)

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

apply ActionPrepareGame =
  tag "Prepare game" $ do
    preparePlayers <-   mconcat . toList
                      . fmap (preparePlayer . view playerId)
                      . view players
                      <$> currentBoard

    apply $
               preparePlayers
            <> mconcat (fmap ActionShuffle [HeroDeck, VillainDeck])
            <> mconcat (fmap replaceHeroInHQ [0..4])
            <> ActionStartTurn

  where
    preparePlayer pid =    ActionShuffle (PlayerLocation pid PlayerDeck)
                        <> ActionDraw pid 6

apply ActionEndTurn = do
  player     <- currentPlayer
  postAction <- view (postDrawActions . at player . non mempty) <$> currentBoard

  tag (playerDesc player <> " turn end") $ do
    board  <-   set
                  (playerResources player)
                  mempty
              . set
                  (postDrawActions . at player)
                  Nothing
              . moveAllFrom
                  (cardsAtLocation $ PlayerLocation player Played)
                  (cardsAtLocation $ PlayerLocation player Discard)
              . moveAllFrom
                  (cardsAtLocation $ PlayerLocation player Hand)
                  (cardsAtLocation $ PlayerLocation player Discard)
              <$> currentBoard

    withBoard board $
      over players moveHeadToTail <$> apply (ActionDraw player 6 <> postAction)

apply ActionStartTurn = do
  pid <- currentPlayer

  tag (playerDesc pid <> " turn start") $ do
    let topCard = cardByIndex VillainDeck 0
    nextCard <- lookupCard topCard

    case view cardTemplate <$> nextCard of
      Nothing -> lose "Draw! No more villains."
      Just TwistCard -> do
        apply $ revealAndMove topCard KO Front <> ActionPlayerTurn pid
      Just MasterStrikeCard -> do
        apply $ revealAndMove topCard KO Front <> ActionPlayerTurn pid
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
          <> revealAndMove (cardByIndex VillainDeck 0) (City 0) Front
          <> ActionPlayerTurn pid
  where
    villainAtLocation board l =
      not . null . view (cardsAtLocation l) $ board

apply (ActionTagged reason action) = tag reason (apply action)

apply a@(ActionChooseCard pid desc options expr pass) = applyChoicesFor pid f
  where
    f (ChoosePass :<| _) = case pass of
                             Nothing     -> f mempty
                             Just action -> return action
    f (ChooseCard location :<| _) = do
      options' <- catMaybes <$> mapM lookupCard options
      card <- requireCard location

      if card `elem` options' then
        do
          board <- currentBoard
          case fromU $ evalWith (mkEnv $ Just board) (UApp expr (UConst $ toU location)) of
             Right x -> return x
             Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y

      else
        f mempty
    f _ = wait a $ playerDesc pid <> ": " <> desc

apply a@(ActionChooseYesNo pid desc onYes onNo) = applyChoicesFor pid f
  where
    f (ChoosePass :<| _) = return onNo
    f (ChooseBool True :<| _) = trace (show onYes) $ return onYes
    f (ChooseBool False :<| _) = return onNo
    f _ = wait a $ playerDesc pid <> ": " <> desc

apply a@(ActionPlayerTurn _) = applyChoicesBoard f
  where
    isTactic MastermindTacticCard{} = True
    isTactic _ = False

    clearAndApply action = do
      pid <- currentPlayer
      board' <- clearChoices <$> currentBoard

      withBoard board' . apply $ action

    f :: S.Seq PlayerChoice -> GameMonad Board
    f (ChooseCard address :<| _) = case cardLocation address of
      PlayerLocation pid' Hand -> do
        pid <- currentPlayer

        if pid == pid' then
          do
            board <- currentBoard
            card  <- requireCard address

            let playAction = fromJust $ preview (cardTemplate . playCode) card

            addressByIndex <- addressById address

            let continue = revealAndMove address (PlayerLocation pid Played) Front
                        <> playAction
            let guardCode = fromJust $ preview (cardTemplate . playGuard) card

            let ret = evalWith
                        (mkEnv $ Just board)
                        (UApp guardCode (UConst . toU $ continue))

            case fromU ret of
              Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y
              Right x -> withCurrentCard addressByIndex $ apply (ActionTagged (playerDesc pid <> " plays " <> view (cardTemplate . cardName) card) $
                           x
                        <> a)
        else
          f mempty

      HQ -> do
        card <- requireCard address
        pid <- currentPlayer
        index <- cardLocationIndex address

        let template = view cardTemplate card

        apply $ ActionTagged (playerDesc pid <> " purchases " <> view cardName template) $
             ActionMove address (PlayerLocation pid Discard) Front
          <> ActionRecruit pid (-(view heroCost template))
          <> replaceHeroInHQ index

      City n -> do
        card <- requireCard address
        pid <- currentPlayer

        let template = view cardTemplate card

        requiredAttack <- evalInt $ view enemyAttack template

        let action = extractCode . fromJust $ preview fightCode template

        apply $ ActionTagged (playerDesc pid <> " attacks " <> view cardName template) $
                           ActionMove address (PlayerLocation pid Victory) Front
                        <> ActionAttack pid (negate requiredAttack)
                        <> action
                        <> a
      MastermindDeck -> do
        mmCards <- view (cardsAtLocation MastermindDeck) <$> currentBoard

        let mmtCards = S.filter (isTactic . view cardTemplate) mmCards

        case toList mmtCards of
          (x:xs) -> do
            let address = cardById MastermindDeck (view cardId x)
            tactic <- requireCard address
            pid <- currentPlayer

            let template = view cardTemplate tactic
            requiredAttack <- evalInt $ view mmtAttack template
            let action = extractCode . fromJust $ preview mmtFightCode template
            let next = case xs of
                        [] -> ActionWin "You defeated the mastermind!"
                        _  -> a

            apply $ ActionTagged (playerDesc pid <> " attacks Mastermind") $
                           ActionMove address (PlayerLocation pid Victory) Front
                        <> ActionAttack pid (negate requiredAttack)
                        <> action
                        <> next
      _ -> f mempty

    f (ChooseEndTurn :<| _) = do
      pid <- currentPlayer

      apply $
           ActionEndTurn
        <> ActionStartTurn
    f _ = do
      pid <- currentPlayer

      wait a $ playerDesc pid <> "'s turn"

apply a@(ActionEval bindings expr) = do
  board <- currentBoard
  card <- currentCard
  let bindings = maybe mempty (M.singleton "current-card" . toUConst) card
  let ret = evalWith (extendEnv bindings . mkEnv . Just $ board) expr

  action <- case fromU ret of
              Right x -> return x
              Left y -> lose $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> y

  apply action

apply a@(ActionLose reason) = lose reason
apply a@(ActionWin reason) = win reason
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
    f (board, as) a =
      withBoard board (do
        board' <- apply a

        return (board', as)) `catchError` handler

      where
        handler (b, e) = return (b, (e, view boardState b):as)

apply a@(ActionDiscard pid) = applyChoicesFor pid discardSelection
  where
    discardSelection (ChooseCard address :<| _) =
      case cardLocation address of
        PlayerLocation pid' Hand | pid == pid' -> do
          card <- requireCard address

          return $ ActionMove
                     address
                     (PlayerLocation pid Discard)
                     Front
        _ -> defaultAction
    discardSelection _ = defaultAction

    defaultAction = return . ActionHalt a $
      playerDesc pid <> ": select a card in hand to discard"

apply ActionKOHero = applyChoices koHero
  where
    koHero (ChooseCard address :<| _)
      | HQ == cardLocation address = do

      pid   <- currentPlayer
      valid <- checkCondition (ConditionCostLTE address 6)
      index <- cardLocationIndex address

      return $ if valid then
             ActionMove address KO Front
          <> revealAndMove (cardByIndex HeroDeck 0) HQ (LocationIndex index)
      else
        ActionHalt ActionKOHero $
          playerDesc pid <> ": select a card in HQ costing 6 or less to KO"
    koHero _ = do
      pid <- currentPlayer

      return . ActionHalt ActionKOHero $
        playerDesc pid <> ": select a card in HQ costing 6 or less to KO"

apply (ActionEndStep a) = do
  player <- currentPlayer

  over (postDrawActions . at player . non mempty) (<> a) <$> currentBoard

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

                            return (board, ActionKOHero <> discardAction)
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

  return (moveAllFrom
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

  let location = cardLocation specificCard

  case playerDeck location of
    Nothing -> lose $ "Card does not exist: " <> showT specificCard
    Just playerId -> do
      let discardDeck = PlayerLocation playerId Discard

      case view (cards . at discardDeck . non mempty) board of
        Empty -> lose $ "No cards left to draw for " <> showT playerId
        cs -> apply $
                ActionTagged
                  (playerDesc playerId <> " shuffles discard into deck")
                  (  moveAndHide (length cs) discardDeck location
                  <> ActionShuffle location)
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
           ActionMove (cardByIndex from 0) to Front
        <> ActionHide (cardByIndex to 0)

overCard :: SpecificCard -> (CardInPlay -> CardInPlay) -> GameMonad Board
overCard address f = do
  let location = cardLocation address
  over (cardsAtLocation location) updateF <$> currentBoard

  where
    updateF xs = case address of
                  CardByIndex (_, i) -> S.adjust' f i xs
                  CardById (_, cid) -> case S.findIndexL (\c -> cid == view cardId c) xs of
                                         Nothing -> error $ "Card does not exist: " <> show address
                                         Just i -> S.adjust' f i xs

halt a = do
  b <- currentBoard

  throwError (b, a)

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility = set cardVisibility

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
lookupCard cardAddress =
  preview (cardAtLocation cardAddress) <$> currentBoard

requireCard :: SpecificCard -> GameMonad CardInPlay
requireCard location = do
  b <- currentBoard
  maybeCard <- lookupCard location

  case maybeCard of
    Just c -> return c
    Nothing -> lose $ "No card at location: " <> showT location

currentPlayerChoices = do
  pid <- currentPlayer

  view (playerChoices . at pid . non mempty) <$> currentBoard

clearChoices =
  set playerChoices mempty

clearChoicesFor pid =
  set (playerChoices . at pid) mempty

applyChoicesBoard f = do
  pid <- currentPlayer

  applyChoicesForBoard pid f

applyChoicesForBoard pid f = do
  choices <- view (playerChoices . at pid . non mempty) <$> currentBoard

  board <- clearChoicesFor pid <$> currentBoard
  withBoard board (f choices)

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

  return $ view (cardTemplate . heroCost) card <= Sum amount

tag :: T.Text -> GameMonad Board -> GameMonad Board
tag message m = do
  state <- currentState
  let (board', actions) = runGameMonad' state m

  case mconcat . toList $ actions of
    ActionNone   -> return ()
    foldedAction -> logAction (ActionTagged message foldedAction)

  return board'

cardLocationIndex :: SpecificCard -> GameMonad Int
cardLocationIndex (CardByIndex (_, i)) = return i
-- TODO: To fix this, this function needs to be moved into board monad
cardLocationIndex address@(CardById (location, cid)) = do
  board <- currentBoard

  case S.findIndexL (\c -> cid == view cardId c) (view (cardsAtLocation location) board) of
    Nothing -> lose $ "No such card: " <> showT address
    Just i -> return i

evalInt (ModifiableInt base modifier) = do
  board <- currentBoard
  let ret = maybe (UInt 0) (evalWith (mkEnv $ Just board)) modifier

  case fromU ret of
    Right x -> return (base <> x)
    Left y -> lose $ "Unexpected state: expr doesn't evaluate to an int. Got: " <> y

addressById address = do
  card <- requireCard address
  return $ cardById (cardLocation address) (view cardId card)
