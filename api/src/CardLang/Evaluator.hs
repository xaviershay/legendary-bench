{-# LANGUAGE OverloadedStrings #-}

module CardLang.Evaluator
  ( eval
  , evalWith
  )
  where

import qualified Data.HashMap.Strict  as M
import Data.Maybe (fromJust)

import Utils

import CardLang.BuiltIn
import CardLang.Types

printValue (UConst x) = case x of
                          (UInt (Sum n)) -> showT n
                          x -> showT x
printValue x = showT x

eval :: UExpr -> UValue
eval = evalWith mempty

evalWith :: UEnv -> UExpr -> UValue
evalWith env exp = evalWith' (builtInEnv <> env) exp

evalWith' :: UEnv -> UExpr -> UValue
evalWith' env (USequence []) = UNone
evalWith' env (USequence [x]) = evalWith' env x
evalWith' env (USequence (x:xs)) = evalWith' env (USequence xs)
evalWith' env (UConst fn@(UFunc env' x body)) = UFunc (env' <> env) x body
evalWith' env (UConst (UList xs)) = UList (map (UConst . evalWith' env) xs)
evalWith' env (UConst v) = v
evalWith' env (UVar label) = evalWith' env $ M.lookupDefault (UConst . UError $ "Unknown variable: " <> label) label env
evalWith' env (ULet (key, value) expr) = evalWith' (M.insert key value env) expr
evalWith' env (UApp fexp arg) =
  case evalWith' env fexp of
    (UFunc env' argname body) -> evalWith' (env <> env') $ ULet (argname, arg) body
    _ -> UError $ (printValue fexp) <> " is not a function"
evalWith' env (UBuiltIn "add") = evalWith' env $ (snd . fromJust $ M.lookup "add" builtIns) $ env

builtInEnv :: UEnv
builtInEnv = M.mapWithKey (typeToFn 0) builtIns
  where
    typeToFn :: Int -> Name -> (MType, UEnv -> UExpr) -> UExpr
    typeToFn n key (WFun a b, f) = UConst $ UFunc mempty ("a" <> showT n) (typeToFn (n+1) key (b, f))
    typeToFn n key (WConst _, _) = UBuiltIn key
