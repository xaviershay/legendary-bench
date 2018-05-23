{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Evaluator where

import           Control.Lens  (at, ix, non, over, set, view, Lens')
import qualified Data.Sequence as S
import qualified Data.Text     as T

import           Action
import           GameMonad
import           Random
import           Types
import           Utils

applyWithVersionBump :: Action -> GameMonad Board
applyWithVersionBump action = do
  b <- apply action

  return $ over version (+ 1) b

-- Applies an action to the current board, returning the resulting one
apply :: Action -> GameMonad Board
apply a@(MoveCard specificCard@(location, i) to dest) = do
  maybeCard <- lookupCard specificCard

  case maybeCard of
    Nothing -> tryShuffleDiscardToDeck a specificCard
    Just card ->    over (cardsAtLocation location) (S.deleteAt i)
                <$> over (cardsAtLocation to) (card S.<|)
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
  board' <- apply a

  if isPlaying board' then
    withBoard board' $ m >>= apply
  else
    return board'

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
    over players moveHeadToTail <$> (apply $ drawAction player 6)

apply action = lose $ "Don't know how to apply: " <> showT action

moveAllFrom :: Lens' Board (S.Seq CardInPlay)
            -> Lens' Board (S.Seq CardInPlay)
            -> Board
            -> Board
moveAllFrom src dest board =
  let cs = view src board in

  set src mempty $ over dest (cs S.><) $ board

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
                       $ set (cardsAtLocation discardDeck) mempty
                       $ set rng rng'
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

lose :: T.Text -> GameMonad Board
lose reason = set boardState (Lost reason) <$> currentBoard

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility v (CardInPlay card _) = CardInPlay card v

invalidResources :: Resources -> Bool
invalidResources r = (view money r < 0) || (view attack r < 0)
