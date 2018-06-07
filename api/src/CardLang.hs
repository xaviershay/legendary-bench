{-# LANGUAGE OverloadedStrings #-}

module CardLang
  ( evalWith
  , evalCards
  , evalWithBoard
  , typecheck
  , parse
  , mkEnv
  )
  where

import qualified Data.HashMap.Strict  as M
import qualified Data.Sequence        as S

import qualified CardLang.Parser
import qualified CardLang.Evaluator
import CardLang.Evaluator (FromU, ToU, toU, argAt)
import qualified CardLang.TypeInference

import CardLang.Types

import Utils
import Types

evalWith :: UEnv -> UExpr -> UValue
evalWith = CardLang.Evaluator.evalWith
evalCards = CardLang.Evaluator.evalCards
evalWithBoard = CardLang.Evaluator.evalWithBoard
typecheck = CardLang.TypeInference.typecheck

parse = CardLang.Parser.parse

mkEnv :: S.Seq Card -> UEnv
mkEnv cards =
  let builtIns = defaultBuiltIns in
    set envCards cards
  $ set envBuiltIn (M.map genBuiltInExpr builtIns)
  $ emptyEnv

genBuiltInExpr :: BuiltInDef -> UExpr
genBuiltInExpr bi = typeToFn 0 bi (view builtInType bi)
  where
    typeToFn :: Int -> BuiltInDef -> MType -> UExpr
    typeToFn n def (WFun _ b) = UConst . UFunc $ UFuncData
                                              { _fnBindings = mempty
                                              , _fnArgName  = ("_a" <> showT n)
                                              , _fnBody     = typeToFn (n+1) def b
                                              , _fnFreeVars = mempty
                                              }
    typeToFn n def _ = UBuiltIn (view builtInName def)

defaultBuiltIns = M.fromList . fmap (\x -> (view builtInName x, x)) $
  [ mkBuiltIn "add" ("Int" ~> "Int" ~> "Int") $ builtInBinOp ((+) :: Int -> Int -> Int)
  ]

mkBuiltIn name t f = BuiltInDef
  { _builtInName = name
  , _builtInType = t
  , _builtInFn = f
  }

builtInBinOp :: (FromU a, ToU b) => (a -> a -> b) -> EvalMonad UExpr
builtInBinOp f = do
  x <- argAt 0
  y <- argAt 1

  return . UConst . toU $ f x y
