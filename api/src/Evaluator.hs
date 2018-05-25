{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import           Control.Lens         (Lens', at, ix, non, over, preview, set,
                                       view)
import           Control.Monad.Except (catchError, throwError)
import qualified Data.Sequence        as S
import Data.Sequence ((<|), (|>), Seq((:<|), Empty))
import qualified Data.Text            as T

import Debug.Trace

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

-- Applies an action to the current board, returning the resulting one.
apply :: Action -> GameMonad Board
apply a@(MoveCard specificCard@(location, i) to dest) = do
  maybeCard <- lookupCard specificCard

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a specificCard
    Just card ->   over (cardsAtLocation location) (S.deleteAt i)
                 . over (cardsAtLocation to) ((insertF dest) card)
                 . over actionLog (\x -> x |> a)
                 <$> currentBoard

  where
    insertF Front = (<|)
    insertF (LocationIndex i) = S.insertAt i

apply a@(RevealCard location v) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> do
      board <- overCard location (setVisibility v)

      return $ over actionLog (\x -> x |> a) board

apply (ApplyResources (PlayerId id) rs) = do
  board <- currentBoard

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

apply (ActionShuffle location) = do
  board <- currentBoard

  let g = view rng board
  let cs = view (cardsAtLocation location) board
  let (shuffled, g') = shuffleSeq g cs

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

  apply $    preparePlayers
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
    over players moveHeadToTail <$> apply (drawAction 6 player)

apply ActionStartTurn = do
  pid <- currentPlayer

  board <- moveCity ActionStartTurn (City 0) mempty

  withBoard board $ do
    board' <- apply $ revealAndMove (VillianDeck, 0) (City 0) Front

    withBoard board' $ apply (ActionPlayerTurn pid)

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

          return $
               revealAndMove location (PlayerLocation pid Played) Front
            <> cardEffect
      else
        f mempty

    f (ChooseCard location@(HQ, i) :<| _) = do
      (CardInPlay card _) <- requireCard location
      pid <- currentPlayer

      return $
           MoveCard location (PlayerLocation pid Discard) Front
        <> ApplyResources pid (mempty { _money = -(cardCost card)})
        <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

    f (ChooseCard location@(City n, i) :<| _) = do
      (CardInPlay card _) <- requireCard location
      pid <- currentPlayer

      return $
           MoveCard location (PlayerLocation pid Victory) Front
        <> ApplyResources pid (mempty { _attack = -(cardHealth card)})

    f (ChooseEndTurn :<| _) = return $ ActionEndTurn <> ActionStartTurn
    f _ = do
      pid <- currentPlayer

      wait a $ playerDesc pid <> "'s turn"

apply a@(ActionLose reason) = lose reason

moveCity :: Action -> Location -> S.Seq CardInPlay -> GameMonad Board
moveCity a Escaped incoming =
  case incoming of
    (CardInPlay card@EnemyCard{} _ :<| other) -> applyChoices f
    _ -> lose "Unexpected incoming in moveCity Escaped handler"

  where
    f (ChooseCard location@(HQ, i) :<| _) = do
      (CardInPlay card _) <- requireCard location

      if cardCost card <= 6 then
        return $
             MoveCard location KO Front
          <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)
      else
        f mempty
    f _ = do
      pid <- currentPlayer

      wait a $ playerDesc pid <> ": select a card in HQ costing 6 or less to KO"

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
                    moveAndHide (length cs) discardDeck location
                <> ActionShuffle location
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
setVisibility v (CardInPlay card _) = CardInPlay card v

invalidResources :: Resources -> Bool
invalidResources r = (view money r < 0) || (view attack r < 0)

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

playerDesc (PlayerId id) = "Player " <> showT id

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
  (CardInPlay card _) <- requireCard location

  return $ cardCost card <= amount
