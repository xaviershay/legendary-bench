{-# LANGUAGE OverloadedStrings #-}

module CardLangTest where

import Test.Tasty
import Test.Tasty.HUnit

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

test_TypeInference = testGroup "Type Inference"
  [ testInfer "Int" "1"
  , testInfer "String" "\"a\""
  , testInfer "Int" "(let [x 1] x)"
  , testInfer "Int" "(let [x (let [y 1] y)] x)"
  , testInfer "a -> Int" "(fn (x) 1)"
  , testInfer "Int" "((fn (x) 1) 2)"
  , testInfer "Int" "(add 1 2)"
  , testInfer "Int -> Int" "(add 1)"
  , testInfer "Int -> Int -> Int" "(fn (x y) (add x y))"
  , testInfer "[a]" "[]"
  , testInfer "[Int]" "[1]"
  , testInfer "[Int]" "[1 2]"
  , testInfer "[Int]" "(let [x 1] [x 2])"
  , testInfer "[Int]" "(let [x 1] [x ((fn (y) y) x)])"
  , testInfer "a -> [a]" "(fn (x) [x])"
  , testInfer "Int" "(def x 1) x"
  , testInfer "Int" "(def y (fn (z) z)) (y 1)"
  , testInfer "Int" "(def y (fn (z) z)) (def x 2) (y 2)"
  , testInfer "Int" "(def y (fn (z) z)) (def x (y 2)) x"
  , testInfer "a -> a" "(def y (fn (z) z)) (def x (y 2)) y"
  ]

testEval = testEvalWith mempty
testEvalWith env expected input =
  testCase (T.unpack input) $ expected @=? query (M.fromList env) input
  where
    query :: UEnv -> Name -> UValue
    query env text = case parse text of
                        Right x -> case typecheck x of
                                     Right _ -> evalWith env x
                                     Left y -> error $ "Typecheck fail: " <> show y
                        Left y -> error $ show y


test_ListQuery = testGroup "List Query"
  [ testEval (UInt 1) "1"
  , testEval (UString "") "\"\""
  , testEval (UString "a") "\"a\""
  , testEval (UString "\"") $ T.pack ['"', '\\', '"', '"']
  , testEvalWith [("x", UConst . UInt $ 0)] (UInt 1) "(let [x 1] x)"
  , testEval (UInt 2) "(let [x 1 y 2] y)"
  , testEval (UInt 1) "((fn (x) x) 1)"
  , testEval (UInt 1) "(let [f (fn () 1)] f)"
  , testEval (UInt 1) "(let [f (fn (x) x)] (f 1))"
  , testEval (UInt 2) "(let [f (fn (x y) y)] (f 1 2))"
  , testEval (UInt 2) "(let [f (fn (x y) y)] ((f 1) 2))"
  , testEval (UInt 1) "(let [] 1)"
  , testEval (UInt 1) "((let [f (fn (x y) x)] (f 1)) 2)"
  , testEval (UInt 1) "(let [y 1] (let [f (fn () y)] f))"
  , testEval (UInt 3) "(add 1 2)"
  , testEval (UInt 3) "(let [x 1] (add x 2))"
  , testEval (UInt 3) "((add 1) 2)"
  , testEval (UList [UConst . UInt $ 1]) "[1]"
  , testEval (UList [UConst . UInt $ 1, UConst . UInt $ 2]) "(let [x 2] [1 x])"
  , testEval (UInt 1) "(def x 1) x"
  , testEval (UInt 2) "(defn foo [x] 2) (foo 1)"
  , testEval (UInt 3) "(defn add-one [x] (add 1 x)) (add-one 2)"
  ]

--focus = defaultMain test_TypeInference
focus = defaultMain $ testGroup "All" [test_ListQuery, test_TypeInference]
