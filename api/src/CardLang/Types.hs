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
import Control.Monad.Reader
import Data.String (IsString, fromString)
import Data.Char (isUpper)

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
  fromString x@(h:_) 
    | isUpper h  = WConst . T.pack $ x
    | True       = WVar . T.pack $ x

type EvalMonad a = (ExceptT T.Text (ReaderT Int (State UEnv))) a
type BuiltIn = (MType, EvalMonad UExpr)
type Bindings = M.HashMap Name UExpr

makeLenses ''UEnv

emptyEnv = UEnv { _envVariables = mempty, _envBoard = Nothing, _envCards = mempty, _envBuiltIn = mempty }
extendEnv :: M.HashMap Name UExpr -> UEnv -> UEnv
extendEnv newVars env = over envVariables (\x -> newVars `M.union` x) env

--instance Monoid UEnv where
--  mempty = UEnv { _envVariables = mempty, _envBoard = Nothing, _envCards = mempty, _envBuiltIn = mempty }
--  -- Union is left biased, so make sure start with b
--  mappend a b = over envVariables (\x -> view envVariables b `M.union` x) a

infixr 8 ~>
(~>) :: MType -> MType -> MType
a ~> b = WFun a b
