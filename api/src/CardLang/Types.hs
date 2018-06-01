module CardLang.Types
  ( module CardLang.Types
  , module Types
  ) where

import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T

import Types (SummableInt(..), Location)

type Name = T.Text

data UExpr =
    UConst UValue
  | UVar Name
  | ULet (Name, UExpr) UExpr
  | UApp UExpr UExpr
  | UBuiltIn Name
  | USequence [UExpr]
  deriving (Show, Eq)

type UEnv = M.HashMap Name UExpr

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
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
