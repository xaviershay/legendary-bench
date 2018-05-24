{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as M
import           Data.List            (nub)
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

type GameHalt = (Board, Action)
type GameMonad a = ExceptT GameHalt (ReaderT GameMonadState Identity) a

type SpecificCard = (Location, Int)
data MoveDestination = Front | Back | LocationIndex Int deriving (Show, Generic)

data PlayerAction =
  PlayCard Int |
  AttackCard Int |
  PurchaseCard Int |
  EndTurn

  deriving (Show, Generic)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq)

data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory
  deriving (Show, Generic, Eq, Enum, Bounded)

data Location = PlayerLocation PlayerId ScopedLocation
  | HQ
  | HeroDeck
  | VillianDeck
  | City Int
  | Escaped
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
  mappend Resources { _attack = a1,      _money = m1 }
          Resources { _attack = a2,      _money = m2 } =
          Resources { _attack = a1 + a2, _money = m1 + m2 }

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

instance Eq CardInPlay where
  (CardInPlay a b) == (CardInPlay c d) = a == c && b == d

type CardMap = M.HashMap Location (S.Seq CardInPlay)

data GameState = Playing | Won | Lost T.Text deriving (Show, Generic, Eq)

newtype Player = Player
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
  { _players       :: S.Seq Player
  , _cards         :: CardMap
  , _boardState    :: GameState
  , _rng           :: StdGen
  , _version       :: Integer
  , _currentAction :: Action
  , _playerChoices :: M.HashMap PlayerId (S.Seq PlayerChoice)
  }
  deriving (Show, Generic)

newtype Game = Game
  { _gameState :: Board
  }
  deriving (Show, Generic)

instance Monoid Effect where
  mempty = EffectNone
  mappend = EffectCombine

data PlayerChoice =
  ChooseCard SpecificCard |
  ChooseEndTurn
  deriving (Show, Generic, Eq)

data Action =
  ActionNone |
  ActionLose T.Text |
  ActionSequence Action (GameMonad Action) |
  MoveCard SpecificCard Location MoveDestination |
  RevealCard SpecificCard Visibility |
  ApplyResources PlayerId Resources |
  ActionPlayerTurn PlayerId |
  ActionStartTurn |
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
  , _currentAction = mempty
  , _rng = mkStdGen 0
  , _version = 1
  , _playerChoices = mempty
  }

-- Return a unique list of all cards in use on the board
cardDictionary :: Board -> [Card]
cardDictionary board =
    nub
  . toList
  . fmap extractCard
  . mconcat
  . fmap (\l -> view (cardsAtLocation l) board)
  $ allLocations board

  where
    extractCard (CardInPlay c _) = c
    allLocations board =
      let playerIds = [0 .. (S.length . view players $ board) - 1] in

         [HQ, HeroDeck, VillianDeck, Escaped, Boss]
      <> allCityLocations
      <> concatMap allPlayerLocations playerIds

    allCityLocations = City <$> [0..4]
    allPlayerLocations playerId =
      PlayerLocation (PlayerId playerId) <$> [(minBound :: ScopedLocation)..]

-- Can't rely on makeLenses'' here because we have different card types and Int
-- doesn't implement Monoid so can't work by default with many of the lens.
-- Could newtype it to fix but probably not worth it.
cardCost :: Card -> Int
cardCost HeroCard { _cost = c } = c
cardCost _ = 0

cardName :: Card -> T.Text
cardName c@HeroCard{}  = view heroName c
cardName c@EnemyCard{} = view enemyName c

cardType :: Card -> T.Text
cardType c@HeroCard{} = "hero"
cardType c@EnemyCard{} = "enemy"

cardHealth :: Card -> Int
cardHealth EnemyCard { _baseHealth = x } = x
cardHealth _ = 0

cardsAtLocation :: Location -> Lens' Board (S.Seq CardInPlay)
cardsAtLocation l = cards . at l . non mempty

playerResources :: PlayerId -> Traversal' Board Resources
playerResources (PlayerId id) = players . ix id . resources

isPlaying :: Board -> Bool
isPlaying board = view boardState board == Playing

extractMoney (EffectMoney n) = n
extractMoney _ = 0

extractAttack (EffectAttack n) = n
extractAttack _ = 0

baseResource f = walk . view playEffect
  where
    walk (EffectCombine a b) = walk a + walk b
    walk x = f x

isLost :: Board -> Bool
isLost board = f $ view boardState board
  where
    f (Lost _) = True
    f _        = False
