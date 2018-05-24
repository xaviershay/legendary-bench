{-# LANGUAGE OverloadedStrings #-}

module JsonTest where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Json

import Data.Aeson (encode, eitherDecode)

test_location =
  let playerLocation = PlayerLocation (PlayerId 123) Hand in

  testGroup "Round-trip JSON of locations"
    [ testCase "player location encode" $
        encode playerLocation @=? "\"player-123-hand\""
    , testCase "player location decode" $
        eitherDecode "\"player-123-hand\"" @=? Right playerLocation
    , testCase "scoped location encode" $
        "\"hand\"" @=? encode Hand
    , testCase "scoped location decode" $
        Right Hand @=? eitherDecode "\"hand\""
    ]
