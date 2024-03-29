{-# LANGUAGE OverloadedStrings #-}

module Unit.CardLang.Eval where

import Unit.Utils

import qualified Data.HashMap.Strict as M
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import Types
import Utils
import CardLang

testEval = testEvalWith mempty
testEvalWith bindings = testEvalFull Nothing bindings mempty
testBoardEval = testEvalFull (Just mkBoard) mempty mempty
testEvalWithPrelude expected input = do
  prelude <- cardsPath "prelude.lisp"
  prelude <- T.readFile prelude

  testEvalFull Nothing mempty (prelude <> "\n") expected input

testDeferredEval expected input =
  let prelude = "" in
  let bindings = mempty in
  let env' = extendEnv (M.fromList bindings) (mkEnv Nothing) in
  return $ testCase (T.unpack . escape $ input) $ expected @=? query env' (prelude <> input)
  where
    query :: UEnv -> Name -> UValue
    query env text = case parse text of
                        Right x -> case typecheck env x of
                                     Right _ ->
                                       let f = evalWith env x in
                                       evalWith (mkEnv $ Just mkBoard) (UConst f)
                                     Left y -> error $ "Typecheck fail: " <> show y
                        Left y -> error $ show y
testEvalFull board bindings prelude expected input =
  let env' = extendEnv (M.fromList bindings) (mkEnv board) in
  return $ testCase (T.unpack . escape $ input) $ expected @=? query env' (prelude <> input)
  where
    query :: UEnv -> Name -> UValue
    query env text = case parse text of
                        Right x -> case typecheck env x of
                                     Right _ -> evalWith env x
                                     Left y -> error $ "Typecheck fail: " <> show y
                        Left y -> error $ show y

test_Eval :: IO TestTree
test_Eval =
  testGroup "CardLang evaluation" <$> sequence
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
  , testBoardEval (UInt . Sum $ 1) "@(1)"
  , testBoardEval (UInt . Sum $ 1) "@(@(1))"
  , testBoardEval (UBool False) "(def x @(1)) @(== 2 x)"
  , testEval (UBoardFunc mempty (UVar "current-player")) "@(current-player)"
  , testEval (UList [UConst . UInt . Sum $ 2, UConst . UInt . Sum $ 3])
      "(defn map [f] (reduce (fn [a x] (concat [a [(f x)]])) [])) (map (add 1) [1 2])"
  , testEval (UList [UConst (UInt (Sum 1))]) "(reduce (fn [a x] (concat [a [x]])) [] [1])"
  , testEval (UInt 4) "((. (fn [a] (add a 1)) (fn [b] (add b 2))) 1)"
  , testEval (UInt 7) "((. (add 1) (add 2) (add 3)) 1)"
  , testEvalWithPrelude (UList [UConst (UInt (Sum 3))]) "(drop 2 [1 2 3])"
  , testEvalWithPrelude (UList []) "(drop 1 [])"
  , testEvalWithPrelude (UList []) "(take 2 [])"
  , testEvalWithPrelude (UBool False) "(any (fn [x] false) [0])"
  , testEvalWithPrelude (UBool True) "(any (fn [x] true) [0])"
  , testEvalWithPrelude (UBool False) "(any (fn [x] true) [])"
  , testEvalWithPrelude (UList [UConst (UInt (Sum 1)), UConst (UInt (Sum 2))]) "(uniq [1 1 2])"
  , testEvalWithPrelude (UList [UConst (UBool False), UConst (UBool True)]) "(uniq [false false true])"
  , testEvalWithPrelude (UList [UConst (UBool False), UConst (UBool False)]) "(replicate 2 false)"
  , testDeferredEval (UList []) "(def x []) (def z @(let [y x] y)) z"
  , testEval (UTuple (UConst . UInt $ 1) (UConst . UBool $ False)) "(tuple 1 false)"
  ]
