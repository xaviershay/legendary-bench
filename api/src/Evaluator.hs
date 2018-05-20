{-# LANGUAGE OverloadedStrings #-}

module Evaluator where

import Control.Lens (view, over, set, ix, non, at)
import Types
import Utils
import qualified Data.Sequence as S

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

apply (ActionNone) = currentBoard
apply (ActionSequence a m) = do
  board' <- apply a

  if isPlaying board' then
    withBoard board' $ m >>= apply
  else
    return board'

apply action = lose $ "Don't know how to apply: " <> showT action

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
          let board' =   set
                           (cardsAtLocation location)
                           (fmap (setVisibility Hidden) cs)
                       $ set (cardsAtLocation discardDeck) mempty
                       $ board

          withBoard board' $ apply a


overCard :: SpecificCard -> (CardInPlay -> CardInPlay) -> GameMonad Board
overCard (location, i) f =
     over (cardsAtLocation location . ix i) f <$> currentBoard
