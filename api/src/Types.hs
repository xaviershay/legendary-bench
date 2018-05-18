{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Monoid (mempty, (<>))

data Effect =
  EffectNone |
  EffectMoney Int |
  EffectAttack Int |
  EffectCombine Effect Effect
  deriving (Show, Generic)

instance Monoid Effect where
  mempty = EffectNone
  mappend a b = EffectCombine a b
  
data Card = Card
  { name :: T.Text
  , effect :: Effect
  }
  deriving (Show, Generic)

data Player = Player
  { hand    :: [Card]
  , played  :: [Card]
  , discard :: [Card]
  , deck    :: [Card]
  , attack  :: Int
  , money   :: Int
  }
  deriving (Show, Generic)

data Board = Board
  { players :: V.Vector Player
  }
  deriving (Show, Generic)

data Game = Game
  { board :: Board
  }
  deriving (Show, Generic)
