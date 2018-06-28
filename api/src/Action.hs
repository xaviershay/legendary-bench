{-# LANGUAGE OverloadedStrings #-}

module Action where

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
     ActionReveal source
  <> ActionMove source destination spot

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
