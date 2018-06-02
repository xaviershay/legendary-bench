{-# LANGUAGE OverloadedStrings #-}

module CardLang.Evaluator
  ( eval
  , evalWith
  , builtIns
  )
  where

import Control.Monad.State (evalState, State, modify, put, get, withState)
import qualified Data.HashMap.Strict  as M
import Data.Maybe (fromJust)

import Utils

import CardLang.Types
import Debug.Trace

printValue (UConst x) = case x of
                          (UInt (Sum n)) -> showT n
                          x -> showT x
printValue x = showT x

eval :: UExpr -> UValue
eval = evalWith mempty

evalWith :: UEnv -> UExpr -> UValue
evalWith env exp = evalState  (evalWith'  exp)(builtInEnv <> env)

evalWith' :: UExpr -> State UEnv UValue
evalWith' (USequence []) = pure UNone
evalWith' (USequence [x]) = evalWith' x
evalWith' (USequence (x:xs)) = do
  evalWith' x
  evalWith' (USequence xs)

evalWith' (UDef name expr) = do
  body <- evalWith' expr
  modify (\x -> M.insert name (UConst body) x)
  pure UNone

evalWith' (UConst fn@(UFunc env' x body)) = do
  env <- get

  pure $ UFunc (env' <> env) x body
evalWith' (UConst (UList xs)) = do
  xs' <- sequenceA (map evalWith' xs)

  pure . UList . map UConst $ xs'

evalWith' (UConst v) = pure $ v
evalWith' (UVar label) = do
  env <- get

  evalWith' $ M.lookupDefault (UConst . UError $ "Unknown variable: " <> label) label env

evalWith' (ULet (key, value) expr) = do
  env <- get

  result <- withState (\x -> M.insert key value x) (evalWith' expr)

  put env
  pure $ result

evalWith' (UApp fexp arg) = do
  fn <- evalWith' fexp

  case fn of
    (UFunc env' argname body) -> do
      modify (\x -> x <> env') >> evalWith' (ULet (argname, arg) body)

    _ -> pure . UError $ (printValue fexp) <> " is not a function"

evalWith' (UIf cond lhs rhs) = do
  UBool result <- evalWith' cond

  if result then
    evalWith' lhs
  else
    evalWith' rhs

evalWith' (UBuiltIn "add") = do
  env <- get

  evalWith' $ (snd . fromJust $ M.lookup "add" builtIns) $ env

builtInEnv :: UEnv
builtInEnv = M.mapWithKey (typeToFn 0) builtIns
  where
    typeToFn :: Int -> Name -> (MType, UEnv -> UExpr) -> UExpr
    typeToFn n key (WFun a b, f) = UConst $ UFunc mempty ("a" <> showT n) (typeToFn (n+1) key (b, f))
    typeToFn n key (WConst _, _) = UBuiltIn key

builtIns :: M.HashMap Name BuiltIn
builtIns = M.fromList
  [ ("add", (WFun (WConst "Int") (WFun (WConst "Int") (WConst "Int")), builtInAdd))
  ]

builtInAdd :: UEnv -> UExpr
builtInAdd env = let
  x = lookupInt "a0"
  y = lookupInt "a1"
  in UConst . UInt $ x + y

  where
    lookupInt name = case M.lookup name env of
                       Nothing -> error $ "Not in env: " <> show name
                       Just x -> case evalWith env x of
                                   UInt x -> x
                                   y -> error $ "Not uint: " <> show y
