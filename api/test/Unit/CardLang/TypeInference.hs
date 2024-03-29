{-# LANGUAGE OverloadedStrings #-}

module Unit.CardLang.TypeInference where

import Unit.Utils

import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import Types
import Utils
import CardLang

testInfer expected input =
  testCase (T.unpack $ input <> " :: " <> expected) $ expected @=? inferType input

  where
    inferType :: T.Text -> T.Text
    inferType text = let result = showType <$> case parse text of
                                          Right x -> typecheck (mkEnv Nothing) x -- (mkTypeEnv builtInDefs) x
                                          Left y  -> error $ "parse error: " <> show y in
                     case result of
                       Right x -> x
                       Left y -> error $ show y

testInferWithPrelude :: T.Text -> T.Text -> IO TestTree
testInferWithPrelude expected input = do
  prelude <- cardsPath "prelude.lisp"
  prelude <- T.readFile prelude

  return $ testCase (T.unpack $ input <> " :: " <> expected) $ expected @=? inferType (prelude <> "\n" <> input)
  where
    inferType :: T.Text -> T.Text
    inferType text = let result = showType <$> case parse text of
                                          Right x -> typecheck (mkEnv Nothing) x -- (mkTypeEnv builtInDefs) x
                                          Left y  -> error $ "parse error: " <> show y in
                     case result of
                       Right x -> x
                       Left y -> error $ show y

test_TypeInference = testGroup "CardLang Type Inference"
  [ testInfer "Int" "1"
  , testInfer "String" "\"a\""
  , testInfer "Bool" "true"
  , testInfer "Bool" "false"
  , testInfer "Int" "(let [x 1] x)"
  , testInfer "Int" "(let [x (let [y 1] y)] x)"
  , testInfer "a -> Int" "(fn [x] 1)"
  , testInfer "Int" "((fn [x] 1) 2)"
  , testInfer "Int" "(add 1 2)"
  , testInfer "Int -> Int" "(add 1)"
  , testInfer "Int -> Int -> Int" "(fn [x y] (add x y))"
  , testInfer "[a]" "[]"
  , testInfer "[Int]" "[1]"
  , testInfer "[Int]" "[1 2]"
  , testInfer "[Int]" "(let [x 1] [x 2])"
  , testInfer "[Int]" "(let [x 1] [x ((fn [y] y) x)])"
  , testInfer "a -> [a]" "(fn [x] [x])"
  , testInfer "Int" "(def x 1) x"
  , testInfer "Int" "(def y (fn [z] z)) (y 1)"
  , testInfer "Int" "(def y (fn [z] z)) (def x 2) (y 2)"
  , testInfer "Int" "(def y (fn [z] z)) (def x (y 2)) x"
  , testInfer "a -> a" "(defn y [z] z) (def x (y 2)) y"
  , testInfer "Int" "(defn foo [x] x) (if (foo false) (foo 1) (foo 2))" -- Requires polymorphic defn
  , testInfer "Int" "(if false 1 2)"
  , testInfer "[Int]" "(if true [1] [])"
  , testInfer "[Int]" "(if true [] [1])"
  , testInfer "Int -> Int" "(fn [x] (if (<= x 0) x x))"
  , testInfer "Int -> Int" "(fn [x] (if (<= x 0) x x))"
  , testInfer "Int" "(reduce (fn [sum n] (add sum n)) 0 [1 2 3])"
  , testInfer "Int" "(reduce (fn [sum n] (add sum n)) 0 [])"
  , testInfer "[Int]" "(reduce (fn [a x] (concat [a [x]])) [] [1])"
  , testInfer "(a -> b) -> [a] -> [b]" "(fn [f xs] (reduce (fn [a x] (concat [a [(f x)]])) [] xs))"
  , testInfer "[Int]" "(concat [[1] []])"
  , testInfer "[Int]" "(concat [[] [1]])"
  , testInfer "[a]" "(concat [[] []])"
  , testInfer "String -> [SpecificCard]" "(fn [scope] (cards-at (player-location current-player scope)))"
  , testInfer "(a -> b) -> [a] -> [b]" "(defn map [f xs] (reduce (fn [a x] (concat [a [(f x)]])) [] xs)) map"
  , testInfer "[a] -> Int" "(defn length [xs] (reduce (fn [a x] (add 1 a)) 0 xs)) length"
  , testInfer "(a -> Bool) -> [a] -> [a]" "(fn [f xs] (reduce (fn [a x] (concat [a (if (f x) [x] [])])) [] xs))"
  , testInfer "(a -> Bool) -> [a] -> [a]" "(fn [f xs] (reduce (fn [a x] (if (f x) [x] [x])) [] xs))"
  , testInfer "(Int -> Bool) -> Int -> Int" "(fn [f x] (if (f x) x 1))"
  , testInfer "[a] -> a -> [a]" "(fn [a x] (concat [a (if (<= 0 1) [x] [])]))"
  , testInfer "Int -> Int" "(. (add 1) (add 2))"
  , testInfer "Int -> Bool" "(. (>= 1) (add 2))"
  , testInfer "[[a]] -> Int" "(. (reduce (fn [a x] 0) 0) concat)"
  , testInfer "Int -> Bool" "(def x (. (>= 1) (add 2))) x"
  , testInfer "Int -> Int" "(defn recur [n] (if (<= n 0) 8 (recur (add n 1) 9))) recur"
  , testInfer "a" "(defn recur [n] recur) (recur 1)"
  , testInfer "@:Int" "@(1)"
  , testInfer "@:Int" "@(@(1))"
  , testInfer "@:Bool" "(def x @(1)) @(== 2 x)"
  , testInfer "@:Int" "(def x @(1)) @(x)"
  , testInfer "(Int, Bool)" "(tuple 1 true)"
  , testInfer "((Int, Int), (Bool, Int))" "(tuple (tuple 1 1) (tuple true 1))"
  ]
