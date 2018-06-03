{-# LANGUAGE TemplateHaskell #-}

module CardLang.Types
  ( module CardLang.Types
  , module Types
  ) where

import           Control.Lens
import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Sequence        as S
import Control.Monad.State
import Control.Monad.Except
import Data.String (IsString, fromString)

import Utils
import Types

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

data UEnv = UEnv
  { _envVariables :: M.HashMap Name UExpr
  , _envBoard :: Maybe Board
  , _envCards :: S.Seq Card
  } deriving (Show)

instance Eq UEnv where
  a == b = True

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
 | UString T.Text
 | UBool Bool
 | UFunc UEnv Name UExpr
 | UBoardFunc UExpr
 | UAction Action
 | UCardTemplate Card
 | UList [UExpr]
 | UError Name
 deriving (Eq, Show)

data MType =
    WVar Name
  | WConst Name
  | WFun MType MType
  | WBoardF MType
  | WList MType

  deriving (Eq, Show)

instance IsString MType where
  fromString = WConst . T.pack

type EvalMonad a = (ExceptT T.Text (State UEnv)) a
type BuiltIn = (MType, EvalMonad UExpr)

makeLenses ''UEnv

instance Monoid UEnv where
  mempty = UEnv { _envVariables = mempty, _envBoard = Nothing, _envCards = mempty }
  mappend a b = over envVariables (view envVariables a <>) b

infixr 8 ~>
(~>) :: MType -> MType -> MType
a ~> b = WFun a b
