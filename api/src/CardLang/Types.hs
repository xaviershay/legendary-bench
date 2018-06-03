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
