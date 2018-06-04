{-# LANGUAGE OverloadedStrings #-}

module CardLangTest where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens (set)
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

import Utils

import CardLang.Evaluator
import CardLang.Types
import CardLang.TypeInference
import CardLang.Parser (parse)

testInfer expected input =
  testCase (T.unpack $ input <> " :: " <> expected) $ expected @=? inferType input

  where
    inferType :: T.Text -> T.Text
    inferType text = let result = showType <$> case parse text of
                                          Right x -> typecheck x
                                          Left y  -> error $ "parse error: " <> show y in
                     case result of
                       Right x -> x
                       Left y -> error $ show y

testInferFail expected input =
  testCase (T.unpack $ input <> " :: FAILS") $ expected @=? inferType input

  where
    inferType :: T.Text -> InferError
    inferType text = let result = showType <$> case parse text of
                                          Right x -> typecheck x
                                          Left y  -> error $ "parse error: " <> show y in
                     case result of
                       Right x -> error $ show x
                       Left y -> y

test_TypeInference = testGroup "Type Inference"
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
  , testInfer "a -> a" "(def y (fn [z] z)) (def x (y 2)) y"
  , testInfer "Int" "(if false 1 2)"
  , testInfer "Int" "(defn foo [x] x) (if (foo false) (foo 1) (foo 2))"
  , testInferFail (CannotUnify "Bool" "Int") "(if false false 2)"
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
  , testInfer "[Int]" "(if true [1] [])"
  , testInfer "[Int]" "(if true [] [1])"
  , testInfer "Int -> Int" "(fn [x] (if (<= x 0) x x))"
  , testInfer "Int -> Int" "(fn [x] (if (<= x 0) x x))"
  ]

-- Replace newlines so test output renders nicely
escape = T.replace "\n" "\\n"

testEval = testEvalWith mempty
testEvalWith env expected input =
  testCase (T.unpack . escape $ input) $ expected @=? query (set envVariables (M.fromList env) mempty) input
  where
    query :: UEnv -> Name -> UValue
    query env text = case parse text of
                        Right x -> case typecheck x of
                                     Right _ -> evalWith env x
                                     Left y -> error $ "Typecheck fail: " <> show y
                        Left y -> error $ show y


test_ListQuery = testGroup "List Query"
  [ testEval (UInt 1) "1"
  , testEval (UInt 1) "; comment\n1"
  , testEval (UString "") "\"\""
  , testEval (UString "a") "\"a\""
  , testEval (UString "\"") "\"\\\"\""
  , testEval (UBool True) "true"
  , testEval (UBool False) "false"
  , testEvalWith [("x", UConst . UInt $ 0)] (UInt 1) "(let [x 1] x)"
  , testEval (UInt 2) "(let [x 1 y 2] y)"
  , testEval (UInt 1) "((fn [x] x) 1)"
  , testEval (UInt 1) "(let [f (fn [] 1)] f)"
  , testEval (UInt 1) "(let [f (fn [x] x)] (f 1))"
  , testEval (UInt 2) "(let [f (fn [x y] y)] (f 1 2))"
  , testEval (UInt 2) "(let [f (fn [x y] y)] ((f 1) 2))"
  , testEval (UInt 1) "(let [] 1)"
  , testEval (UInt 1) "((let [f (fn [x y] x)] (f 1)) 2)"
  , testEval (UInt 1) "(let [y 1] (let [f (fn [] y)] f))"
  , testEval (UInt 3) "(add 1 2)"
  , testEval (UInt 10) "(add 1 (add 4 5))"
  , testEval (UInt 3) "(let [x 1] (add x 2))"
  , testEval (UInt 3) "((add 1) 2)"
  , testEval (UList [UConst . UInt $ 1]) "[1]"
  , testEval (UList [UConst . UInt $ 1, UConst . UInt $ 2]) "(let [x 2] [1 x])"
  , testEval (UInt 1) "(def x 1) x"
  , testEval (UInt 2) "(defn foo [x] 2) (foo 1)"
  , testEval (UInt 3) "(defn add-one [x] (add 1 x)) (add-one 2)"
  , testEval (UInt 1) "(def x \"comment\" 1) x"
  , testEval (UInt 2) "(defn foo [x] \"comment\" 2) (foo 1)"
  , testEval (UInt 1) "((fn [x] 2 \"comment\" x) 1)"
  , testEval (UInt 1) "(let [x 1] 2 \"comment\" x)"
  , testEval (UInt 3) "(let [x 1] (defn y [z] (add z x)) (y 2))"
  , testEval (UInt 1) "(if true 1 2)"
  , testEval (UInt 2) "(if false 1 2)"
  , testEval (UInt 6) "(reduce (fn [sum n] (add sum n)) 0 [1 2 3])"
  , testEval (UList [UConst $ UInt 1, UConst $ UInt 2]) "(concat [[1] [2]])"
  , testEval (UBoardFunc mempty (UConst . UInt . Sum $ 1)) "(board-fn 1)"
  , testEval (UBoardFunc mempty (UConst . UInt . Sum $ 1)) "@(1)"
  , testEval (UBoardFunc mempty (UVar "current-player")) "@(current-player)"
  , testEval (UList [UConst . UInt . Sum $ 2, UConst . UInt . Sum $ 3])
      "(defn map [f] (reduce (fn [a x] (concat [a [(f x)]])) [])) (map (add 1) [1 2])"
  , testEval (UList [UConst (UInt (Sum 1))]) "(reduce (fn [a x] (concat [a [x]])) [] [1])"
  ]

focus = defaultMain test_TypeInference
--focus = defaultMain $   testInfer "(a -> Bool) -> [a] -> [a]" "(fn [f xs] (reduce (fn [a x] (if (f x) [x] [])) [] xs))"
--focus = defaultMain $ testInfer "Int -> Int" "(fn [x] (if (<= x 0) x x))"
--focus = defaultMain $ testGroup "All" [test_ListQuery, test_TypeInference]
--focus = defaultMain $ testInfer "[Int]" "(reduce (fn [a x] (concat [a [x]])) [] [1])"
--
-- NOTE: this is wrong. Should be [a] -> a -> [a]
--focus = defaultMain $ testInfer "[a] -> a -> [a]" "(fn [a x] (concat [a [x]]))"
--
-- NOTE: a -> [b] here: should be a -> [a] right?
--focus = defaultMain $ testInfer "a -> [b]" "((fn [a x] (concat [a [x]])) [])"
--focus = defaultMain $ testInfer "[a]" "(((fn [a x] (concat [a [x]])) []) [1])"
--focus = defaultMain $ testInfer "[Int]" "(concat [[1] [2]])"
--focus = defaultMain $ testInfer "[[a]] -> [a]" "(concat [1 [2]])"
