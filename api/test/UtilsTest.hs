{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module UtilsTest where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Sequence as S

import Utils

test_moveHeadToTail = testGroup "moveHeadToTail"
  [ testProperty "maintains length" $
      \(xs :: S.Seq Int) -> length xs == length (moveHeadToTail xs)
  , testProperty "matches list model" $ \case
      [] -> True
      ((x :: Int):xs) -> xs ++ [x] ==
                         toList (moveHeadToTail $ S.fromList (x:xs))
  ]
