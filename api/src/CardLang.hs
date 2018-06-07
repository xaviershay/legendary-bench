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
import qualified CardLang.BuiltIn as B

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
genBuiltInExpr bi = typeToFn 0 bi mtype
  where
    (Forall _ mtype) = view builtInType bi

    typeToFn :: Int -> BuiltInDef -> MType -> UExpr
    typeToFn n def (WFun _ b) = UConst . UFunc $ UFuncData
                                              { _fnBindings = mempty
                                              , _fnArgName  = ("_a" <> showT n)
                                              , _fnBody     = typeToFn (n+1) def b
                                              , _fnFreeVars = mempty
                                              }
    typeToFn n def _ = UBuiltIn (view builtInName def)

defaultBuiltIns = M.fromList . fmap (\x -> (view builtInName x, x)) $
  [ mkBuiltIn "add" ("Int" ~> "Int" ~> "Int") $ B.binOp ((+) :: Int -> Int -> Int)
  , mkBuiltIn "<=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<=) :: Int -> Int -> Bool)
  , mkBuiltIn ">=" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>=) :: Int -> Int -> Bool)
  , mkBuiltIn "<" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((<) :: Int -> Int -> Bool)
  , mkBuiltIn ">" ("Int" ~> "Int" ~> "Bool") $ B.binOp ((>) :: Int -> Int -> Bool)
  , mkBuiltIn "==" ("a" ~> "a" ~> "Bool") $ B.binOp ((==) :: UValue -> UValue -> Bool)
  , mkBuiltIn "reduce" (("b" ~> "a" ~> "b") ~> "b" ~> WList "a" ~> "b")  B.reduce
  , mkBuiltIn "concat" (WList (WList "x") ~> WList "x") B.concat
  , mkBuiltIn "combine" ("Action" ~> "Action" ~> "Action") $ B.binOp ((<>) :: Action -> Action -> Action)
  ]

mkBuiltIn name t f = BuiltInDef
  { _builtInName = name
  , _builtInType = ptype
  , _builtInFn = f
  }
  where
    ptype = CardLang.TypeInference.generalize mempty t

