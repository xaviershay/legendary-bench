{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import           Control.Lens         (Lens', at, ix, non, over, preview, set,
                                       view)
import           Control.Monad.Except (catchError, throwError)
import qualified Data.Sequence        as S
import qualified Data.Text            as T

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
                 . over (cardsAtLocation to) (card S.<|)
                 <$> currentBoard

apply a@(RevealCard location v) = do
  maybeCard <- lookupCard location

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a location
    Just card -> overCard location (setVisibility v)

apply (ApplyResources (PlayerId id) rs) = do
  board <- currentBoard

  let board' = over (players . ix id . resources) (rs <>) board

  if invalidResources (view (players . ix id . resources) board') then
    lose "Not enough resources"
  else
    return board'

apply ActionNone = currentBoard

apply (ActionSequence a m) = do
  board' <- apply a `catchError` handler

  withBoard board' $ m >>= apply

  where
    handler (board, action) = throwError (board, ActionSequence action m)

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
    over players moveHeadToTail <$> apply (drawAction player 6)

apply ActionStartTurn = do
  -- make space for new villian
  -- move villian from 0 to 1, if 1 is full move from 1 to 2, recurse until
  -- empty or escaped
  player <- currentPlayer

  board <- moveCity (City 0)

  withBoard board (apply $ revealAndMove (VillianDeck, 0) (City 0) Front)

apply a@(ActionPlayerTurn playerId) = do
  board <- currentBoard

  let choices = view (playerChoices . at playerId . non mempty) board

  board' <- f choices

  return $ set (playerChoices . at playerId) mempty board'

  where
    f :: S.Seq PlayerChoice -> GameMonad Board
    f (ChooseCard location@(PlayerLocation playerId Hand, i) S.:<| _) = do
      card       <- requireCard location
      cardEffect <- playAction card

      apply $
           revealAndMove location (PlayerLocation playerId Played) Front
        <> cardEffect

    f (ChooseCard location@(HQ, i) S.:<| _) = do
      (CardInPlay card _) <- requireCard location

      apply $
           MoveCard location (PlayerLocation playerId Discard) Front
        <> ApplyResources playerId (mempty { _money = -(cardCost card)})
        <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

    f (ChooseCard location@(City n, i) S.:<| _) = do
      (CardInPlay card _) <- requireCard location

      apply $
           MoveCard location (PlayerLocation playerId Victory) Front
        <> ApplyResources playerId (mempty { _attack = -(cardHealth card)})

    f (ChooseEndTurn S.:<| _) =
      apply $ ActionEndTurn <> ActionStartTurn
    f _ = halt a


apply a@(ActionLose reason) = lose reason

moveCity :: Location -> GameMonad Board
moveCity Escaped = currentBoard -- TODO: Apply penalty for villian escaping
moveCity location@(City i) = do
  recurse <- not . S.null . view (cardsAtLocation location) <$> currentBoard

  let nextLocation = nextL location

  board <- if recurse then
             moveCity nextLocation
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

  set src mempty . over dest (cs S.><) $ board

tryShuffleDiscardToDeck :: Action -> SpecificCard -> GameMonad Board
tryShuffleDiscardToDeck a specificCard = do
  board <- currentBoard

  let (location, _) = specificCard

  case playerDeck location of
    Nothing -> lose $ "Card does not exist: " <> showT specificCard
    Just playerId -> do
      let discardDeck = PlayerLocation playerId Discard

      case view (cards . at discardDeck . non mempty) board of
        S.Empty -> lose $ "No cards left to draw for " <> showT playerId
        cs -> do
          let (shuffled, rng') = shuffleSeq (view rng board) cs
          let board' =   set
                           (cardsAtLocation location)
                           (fmap (setVisibility Hidden) shuffled)
                       . set (cardsAtLocation discardDeck) mempty
                       . set rng rng'
                       $ board

          withBoard board' $ apply a

  where
    -- Returns the relevant PlayerId if the given location is a player's deck.
    -- This is oddly specific so there's probably a better way to structure the
    -- above code.
    playerDeck :: Location -> Maybe PlayerId
    playerDeck (PlayerLocation playerId PlayerDeck) = Just playerId
    playerDeck _ = Nothing

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

  throwError (set boardState (Lost reason) $ b, ActionLose reason)

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
