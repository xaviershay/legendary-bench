{-# LANGUAGE OverloadedStrings #-}

module Action where

import qualified Data.Sequence        as S
import qualified Data.Text as T
import Control.Monad.Except (throwError)

import Types
import Utils
import GameMonad
import Control.Lens

-- Action Builders
-- ===============

replaceHeroInHQ i = ActionTagged ("Replace hero in spot " <> showT i) $
  revealAndMove (specificCardByIndex HeroDeck 0) HQ (LocationIndex i)

revealAndMove source destination spot =
     ActionVisibility source All
  <> ActionMove source destination spot

gainAction :: PlayerId -> SpecificCard -> GameMonad Action
gainAction pid address = do
  index <- cardLocationIndex address

  return $    ActionMove address (PlayerLocation pid Discard) Front
           <> replaceHeroInHQ index

-- Stuff that is probably in the wrong place
-- =========================================
currentPlayer :: GameMonad PlayerId
currentPlayer = do
  pid <- preview (players . element 0 . playerId) <$> currentBoard

  case pid of
    Just x -> return x
    _ -> lose "No active player"

lose :: T.Text -> GameMonad a
lose reason = do
  b <- currentBoard

  throwError (set boardState (Lost reason) b, ActionLose reason)

win :: T.Text -> GameMonad a
win reason = do
  b <- currentBoard

  throwError (set boardState (Won reason) b, ActionWin reason)

cardLocationIndex :: SpecificCard -> GameMonad Int
cardLocationIndex (CardByIndex (_, i)) = return i
cardLocationIndex address@(CardById (location, cid)) = do
  board <- currentBoard

  case S.findIndexL (\c -> cid == view cardId c) (view (cardsAtLocation location) board) of
    Nothing -> lose $ "No such card: " <> showT address
    Just i -> return i
