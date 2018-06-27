{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.QuickCheck (Small(..))

import Control.Lens
import qualified Data.Sequence as S
import Data.Foldable (toList)

import Types
import Evaluator
import GameMonad
import Action
import Utils

import Debug.Trace

--genCards n = S.replicate n $ buildCard moneyCard All

buildCard template vis = CardInPlay
  { _cardTemplate = template
  , _cardVisibility = vis
  , _cardId = CardId 0
  }

test_ConcurrentAction =
  testGroup "Concurrent action"
    [ concurrentTest1
    , concurrentTest2
    ]
  where
    tracer x = ActionTrace x
    halt x = ActionHalt (tracer x) x

    concurrentTest1 = testCase "Applys all actions" $
      [tracer "a", tracer "b"]  @=? appliedAction
      where
        appliedAction = toList $ view actionLog result
        action = ActionConcurrent [tracer "a", tracer "b"]
        result = runGameMonad mkBoard $ apply action

    concurrentTest2 = testCase "Halts if any actions halt, but applies other actions" $
      ( ActionConcurrent [tracer "a", tracer "c"]
      , [tracer "b"]
      , WaitingForChoice "a, c"
      )  @=? (resume, appliedAction, state)
      where
        state = view boardState result
        resume = view currentAction result
        appliedAction = toList $ view actionLog result
        result = runGameMonad mkBoard $ apply action
        action = ActionConcurrent [halt "a", tracer "b", halt "c"]

--test_DrawFromEmpty =
--  testGroup "Drawing from empty deck shuffles in discard"
--    [ testCase "Hand contains a card" $ 1 @=? lengthOf Hand
--    , testCase "Discard is empty" $ 0 @=? lengthOf Discard
--    ]
--
--  where
--    board = set (cardsAtLocation (PlayerLocation player Discard)) (genCards 1)
--              $ mkBoard
--    action = drawAction 1 player
--    player = PlayerId 0
--    result = runGameMonad board $ apply action
--    lengthOf x = length $ view (cardsAtLocation (PlayerLocation player x)) result

addPlayer player = over players (mkPlayer player S.<|)
