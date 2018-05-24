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
import Cards
import Action
import Utils

import Debug.Trace

genCards n = S.replicate n (CardInPlay moneyCard All)

test_DrawFromEmpty =
  testGroup "Drawing from empty deck shuffles in discard"
    [ testCase "Hand contains a card" $ 1 @=? lengthOf Hand
    , testCase "Discard is empty" $ 0 @=? lengthOf Discard
    ]

  where
    board = set (cardsAtLocation (PlayerLocation player Discard)) (genCards 1)
              $ mkBoard
    action = drawAction player 1
    player = PlayerId 0
    result = runGameMonad board $ apply action
    lengthOf x = length $ view (cardsAtLocation (PlayerLocation player x)) result

test_SpiderMan =
  testGroup "Spiderman works when deck is emty"
    [ testCase "Hand contains a card" $ 1 @=? lengthOf Hand
    , testCase "Discard is empty" $ 0 @=? lengthOf Discard
    ]

  where
    choice = ChooseCard (PlayerLocation player Hand, 0)
    player = PlayerId 0
    board =   set
                (cardsAtLocation (PlayerLocation player Discard))
                (genCards 1)
            . set
                (cardsAtLocation (PlayerLocation player Hand))
                (S.fromList [CardInPlay spideyCard Owner])
            . addPlayer player
            . addChoice player choice
            $ mkBoard
    result = runGameMonad board $ apply (ActionPlayerTurn player)
    lengthOf x = length $ view (cardsAtLocation (PlayerLocation player x)) result

test_SpiderManLose =
  testCase "Spiderman loses if deck and discard are empty" $
    True @=? (isLost result)

  where
    choice = ChooseCard (PlayerLocation player Hand, 0)
    player = PlayerId 0
    board =
              set
                (cardsAtLocation (PlayerLocation player Hand))
                (S.fromList [CardInPlay spideyCard Owner])
            . addPlayer player
            . addChoice player choice
            $ mkBoard
    result = runGameMonad board $ apply (ActionPlayerTurn player)

addPlayer player = over players (mkPlayer player S.<|)
