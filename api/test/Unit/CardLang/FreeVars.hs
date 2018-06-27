{-# LANGUAGE OverloadedStrings #-}

module Unit.CardLang.FreeVars where

import Unit.Utils

import qualified Data.Text     as T

import Types
import CardLang
import CardLang.Evaluator (freeVars)

testFreeVars expected input =
  testCase (T.unpack . escape $ input) $ expected @=? fv input

  where
    fv :: T.Text -> [Name]
    fv input = case parse input of
           Right x -> toList . freeVars emptyEnv $ x
           Left y -> error $ show y

test_FreeVariables = testGroup "CardLang Free vars"
  [ testFreeVars [] "1"
  , testFreeVars ["x"] "x"
  , testFreeVars [] "(let [x 1] x)"
  , testFreeVars ["x", "y"] "(x y)"
  , testFreeVars ["y"] "(let [x 1] (x y))"
  , testFreeVars ["x"] "[x 1]"
  , testFreeVars ["y"] "(fn [x] (x y))"
  , testFreeVars ["x"] "@(x)"
  , testFreeVars ["a"] "(let [x 1] (fn [y z] (x z a)))"
  ]
