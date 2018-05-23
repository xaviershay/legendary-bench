{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as M
import qualified Data.Sequence        as S
import qualified Data.Text            as T
import           GHC.Generics
import           System.Random        (StdGen, mkStdGen)

import Debug.Trace

import Utils

data GameMonadState = GameMonadState
  { _activePlayer :: PlayerId
  , _board        :: Board
  }

type GameMonad = Reader GameMonadState

type SpecificCard = (Location, Int)
data MoveDestination = Front | Back | LocationIndex Int deriving (Show, Generic)

data PlayerAction =
  PlayCard Int |
  PurchaseCard Int |
  EndTurn

  deriving (Show, Generic)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq)

data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory deriving (Show, Generic, Eq)
data Location = PlayerLocation PlayerId ScopedLocation
  | HQ
  | HeroDeck
  | Boss
  deriving (Show, Generic, Eq)
newtype PlayerId = PlayerId Int deriving (Show, Generic, Eq)


data Card = HeroCard
  { _heroName   :: T.Text
  , _playEffect :: Effect
  , _cost       :: Int
  } | EnemyCard
  { _enemyName :: T.Text
  , _baseHealth :: Int
  }

  deriving (Show, Generic)

data CardInPlay = CardInPlay Card Visibility deriving (Show, Generic)

data Resources = Resources
  { _attack :: Int
  , _money  :: Int
  } deriving (Show, Generic, Eq)

instance Monoid Resources where
  mempty = Resources { _attack = 0,       _money = 0 }
  mappend (Resources { _attack = a1,      _money = m1 })
          (Resources { _attack = a2,      _money = m2 }) =
           Resources { _attack = a1 + a2, _money = m1 + m2 }

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

instance Eq CardInPlay where
  (CardInPlay a b) == (CardInPlay c d) = a == c && b == d

type CardMap = M.HashMap Location (S.Seq CardInPlay)

data GameState = Playing | Won | Lost T.Text deriving (Show, Generic, Eq)

data Player = Player
  { _resources :: Resources
  }
  deriving (Show, Generic, Eq)

data Effect =
  EffectNone |
  EffectMoney Int |
  EffectAttack Int |
  EffectCustom T.Text (GameMonad Action) |
  EffectCombine Effect Effect
  deriving (Generic)

instance Show Effect where
  show EffectNone = ""
  show (EffectMoney n) = "Money +" <> show n
  show (EffectAttack n) = "Attack +" <> show n
  show (EffectCombine EffectNone EffectNone) = show ""
  show (EffectCombine EffectNone b) = show b
  show (EffectCombine a EffectNone) = show a
  show (EffectCombine a b) = show a <> ", " <> show b
  show (EffectCustom a _) = T.unpack a

data Board = Board
  { _players :: S.Seq Player
  , _cards   :: CardMap
  , _boardState   :: GameState
  , _rng :: StdGen
  , _version :: Integer
  }
  deriving (Show, Generic)

data Game = Game
  { _gameState :: Board
  }
  deriving (Show, Generic)

instance Monoid Effect where
  mempty = EffectNone
  mappend a b = EffectCombine a b

data Action =
  ActionNone |
  ActionLose T.Text |
  ActionSequence Action (GameMonad Action) |
  MoveCard SpecificCard Location MoveDestination |
  RevealCard SpecificCard Visibility |
  ApplyResources PlayerId Resources |
  ActionEndTurn

  deriving (Generic)

instance Show Action where
  show ActionNone = "None"
  show (ActionLose s) = "Lose: " <> T.unpack s
  show (ActionSequence a f) = "Sequence"
  show _ = "Some Action"

instance Monoid Action where
  mempty = ActionNone
  mappend a b = ActionSequence a (return b)

makeLenses ''Player
makeLenses ''Board
makeLenses ''Card
makeLenses ''Resources
makeLenses ''Game

-- This instance is used for anything substantial, it's just needed for some
-- lens derivation (which we ultimately don't rely on)
instance Eq Card where
  a == b = cardType a == cardType b && cardName a == cardName b

mkBoard :: Board
mkBoard = Board
  { _players = mempty
  , _boardState = Playing
  , _cards = mempty
  , _rng = mkStdGen 0
  , _version = 1
  }

-- Can't rely on makeLenses'' here because we have different card types and Int
-- doesn't implement Monoid so can't work by default with many of the lens.
-- Could newtype it to fix but probably not worth it.
cardCost :: CardInPlay -> Int
cardCost (CardInPlay (HeroCard { _cost = c }) _) = c
cardCost _ = 0

cardName :: Card -> T.Text
cardName (c@HeroCard{})  = view heroName c
cardName (c@EnemyCard{}) = view enemyName c

cardType :: Card -> T.Text
cardType (c@HeroCard{}) = "hero"
cardType (c@EnemyCard{}) = "enemy"

cardsAtLocation :: Location -> Lens' Board (S.Seq CardInPlay)
cardsAtLocation l = cards . at l . non mempty

playerResources :: PlayerId -> Traversal' Board Resources
playerResources (PlayerId id) = players . ix id . resources

isPlaying :: Board -> Bool
isPlaying board = view boardState board == Playing

isLost :: Board -> Bool
isLost board = f $ view boardState board
  where
    f (Lost _) = True
    f _        = False
