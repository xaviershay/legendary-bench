module CardLang.Types
  ( module CardLang.Types
  , module Types
  ) where

import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import Data.String (IsString, fromString)

import Types (SummableInt(..), Location)

type Name = T.Text

data UExpr =
    UConst UValue
  | UVar Name
  | ULet (Name, UExpr) UExpr
  | UDef Name UExpr
  | UApp UExpr UExpr
  | UBuiltIn Name
  | UIf UExpr UExpr UExpr
  | USequence [UExpr]
  deriving (Show, Eq)

type UEnv = M.HashMap Name UExpr

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
 | UString T.Text
 | UBool Bool
 | UFunc UEnv Name UExpr
 | UList [UExpr]
 | UError Name
 deriving (Eq, Show)

data MType =
    WVar Name
  | WConst Name
  | WFun MType MType
  | WList MType

  deriving (Eq, Show)

instance IsString MType where
  fromString = WConst . T.pack

type BuiltIn = (MType, UEnv -> UExpr)
