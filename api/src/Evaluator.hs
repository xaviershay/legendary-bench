{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import           Control.Lens         (Lens', at, ix, non, over, preview, set,
                                       view)
import           Control.Monad.Except (catchError, throwError)
import           Control.Monad.Writer (tell)
import qualified Data.Sequence        as S
import Data.Sequence ((<|), (|>), Seq((:<|), Empty))
import qualified Data.Text            as T

import Debug.Trace

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

logAction :: Action -> GameMonad ()
logAction a = tell (S.singleton a)

-- Applies an action to the current board, returning the resulting one.
apply :: Action -> GameMonad Board
apply a@(MoveCard specificCard@(location, i) to dest) = do
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
    insertF Front = (<|)
    insertF (LocationIndex i) = S.insertAt i

apply a@(RevealCard location v) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> do
      board <- overCard location (setVisibility v)

      logAction a

      return board

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
  preparePlayers <-   mconcat . toList
                    . fmap (preparePlayer . view playerId)
                    . view players
                    <$> currentBoard

  apply . ActionTagged "Prepare game" $
             preparePlayers
          <> (mconcat $ fmap ActionShuffle [HeroDeck, VillianDeck])
          <> ActionStartTurn

  where
    preparePlayer pid =    ActionShuffle (PlayerLocation pid PlayerDeck)
                        <> drawAction 6 pid

apply ActionEndTurn = do
  player <- currentPlayer
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
    over players moveHeadToTail <$> apply
      (ActionTagged (playerDesc player <> " turn end") $ drawAction 6 player)

apply ActionStartTurn = do
  pid <- currentPlayer

  board <- moveCity (ActionTagged (playerDesc pid <> " turn start") ActionStartTurn) (City 0) mempty

  withBoard board $ do
    board' <- apply $
                revealAndMove (VillianDeck, 0) (City 0) Front

    withBoard board' $ apply (ActionPlayerTurn pid)

apply a@(ActionTagged tag subAction) = do
  board <- currentBoard

  let (board', actions) = runGameMonad' board (apply subAction)
  case mconcat . toList $ actions of
    ActionNone   -> return ()
    foldedAction -> logAction (ActionTagged tag foldedAction)

  return board'

apply a@(ActionPlayerTurn _) = applyChoices f
  where
    clearAndApply action = do
      pid <- currentPlayer
      board' <- clearChoicesFor pid <$> currentBoard

      withBoard board' . apply $ action

    f :: S.Seq PlayerChoice -> GameMonad Action
    f (ChooseCard location@(PlayerLocation pid' Hand, i) :<| _) = do
      pid <- currentPlayer

      if pid == pid' then
        do
          card       <- requireCard location
          cardEffect <- playAction card

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
        <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

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
        <> ActionTagged (playerDesc pid <> " turn start") ActionStartTurn
    f _ = do
      pid <- currentPlayer

      wait a $ playerDesc pid <> "'s turn"

apply a@(ActionLose reason) = lose reason
apply (ActionHalt a reason) = wait a reason

moveCity :: Action -> Location -> S.Seq CardInPlay -> GameMonad Board
moveCity a Escaped incoming =
  case incoming of
    (card :<| other) -> case view cardTemplate card of
                          EnemyCard{} -> applyChoices villainEscaped
                          _ -> lose "Unhandled incoming in Escaped handler"
    _                -> lose "No incoming cards in Escaped handler"

  where
    haltAction = do
      pid <- currentPlayer

      return . ActionHalt a $
        playerDesc pid <> ": select a card in HQ costing 6 or less to KO"

    villainEscaped (ChooseCard location@(HQ, i) :<| _) = do
      elseAction <- haltAction

      -- TODO: Handle case where no cards matching criteria are in HQ
      return $ ActionIf
        (ConditionCostLTE location 6)
          (  MoveCard location KO Front
          <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)
          )
        elseAction
    villainEscaped _ = haltAction

moveCity a location@(City i) incoming = do
  cardsHere <- view (cardsAtLocation location) <$> currentBoard

  let recurse = not . S.null $ cardsHere
  let nextLocation = nextL location

  board <- if recurse then
             moveCity a nextLocation cardsHere
           else
             currentBoard

  return $ moveAllFrom
             (cardsAtLocation location)
             (cardsAtLocation nextLocation)
             board

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
overCard (location, i) f =
     over (cardsAtLocation location . ix i) f <$> currentBoard


halt a = do
  b <- currentBoard

  throwError (b, a)

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility v = set cardVisibility v

invalidResources :: Resources -> Bool
invalidResources r = (view money r < mempty) || (view attack r < mempty)

lose :: T.Text -> GameMonad a
lose reason = do
  b <- currentBoard

  throwError (set boardState (Lost reason) b, ActionLose reason)

wait :: Action -> T.Text -> GameMonad a
wait a reason = do
  b <- currentBoard

  throwError (set boardState (WaitingForChoice reason) b, a)

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) =
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

clearChoicesFor pid =
  set (playerChoices . at pid) mempty

applyChoices f = do
  choices <- currentPlayerChoices
  a' <- f choices
  clearAndApply a'

  where
    clearAndApply action = do
      playerId <- currentPlayer
      board' <- clearChoicesFor playerId <$> currentBoard

      withBoard board' . apply $ action

checkCondition :: Condition -> GameMonad Bool
checkCondition (ConditionCostLTE location amount) = do
  card <- requireCard location

  return $ view (cardTemplate . heroCost) card <= (Sum amount)
