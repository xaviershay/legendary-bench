{-# LANGUAGE OverloadedStrings #-}

module Unit.Utils
  ( escape
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Utils
  )
  where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text            as T

import Utils

-- Replace newlines so test output renders nicely
escape = T.replace "\n" "\\n"
