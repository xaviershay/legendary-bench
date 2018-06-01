{-# LANGUAGE OverloadedStrings #-}

module CardLang.BuiltIn (builtIns) where

import qualified Data.HashMap.Strict  as M

import CardLang.Types

type BuiltIn = (MType, UEnv -> UExpr)

builtIns :: M.HashMap Name BuiltIn
builtIns = M.fromList
  [ ("add", (WFun (WConst "Int") (WFun (WConst "Int") (WConst "Int")), builtInAdd))
  ]

builtInAdd :: UEnv -> UExpr
builtInAdd env = let
  Just (UConst (UInt x)) = M.lookup "a0" env
  Just (UConst (UInt y)) = M.lookup "a1" env
  in UConst . UInt $ x + y
