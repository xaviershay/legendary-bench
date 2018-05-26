{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Writer (WriterT)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as M
import           Data.List            (nub)
import qualified Data.Sequence        as S
import qualified Data.Text            as T
import           GHC.Generics hiding (to)
import           System.Random        (StdGen, mkStdGen)

import Debug.Trace

import Utils

data GameMonadState = GameMonadState
  { _board        :: Board
  }

type GameHalt = (Board, Action)
type GameMonad a = (ExceptT GameHalt (ReaderT GameMonadState (WriterT (S.Seq Action) Identity))) a

type SpecificCard = (Location, Int)
data MoveDestination = Front | LocationIndex Int deriving (Show, Generic)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq, Bounded, Enum)

data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory
  deriving (Show, Generic, Eq, Enum, Bounded)

newtype HeroType = HeroType T.Text
  deriving (Show, Generic, Eq, Monoid)

newtype HeroTeam = HeroTeam T.Text
  deriving (Show, Generic, Eq, Monoid)

data Location = PlayerLocation PlayerId ScopedLocation
  | HQ
  | KO
  | HeroDeck
  | VillainDeck
  | City Int
  | Escaped
  | Boss
  deriving (Show, Generic, Eq)
newtype PlayerId = PlayerId Int deriving (Show, Generic, Eq)


data Card = HeroCard
  { _heroName   :: T.Text
  , _heroAbilityName :: T.Text
  , _heroType :: HeroType
  , _heroTeam :: HeroTeam
  , _playEffect :: Effect
  , _heroCost   :: SummableInt
  } | EnemyCard
  { _enemyName :: T.Text
  , _baseHealth :: SummableInt
  }

  deriving (Show, Generic)

newtype CardId = CardId Int deriving (Show, Generic, Eq)

data CardInPlay = CardInPlay
  { _cardId :: CardId
  , _cardVisibility :: Visibility
  , _cardTemplate :: Card
  } deriving (Show, Generic)

newtype SummableInt = Sum Int deriving (Show, Generic, Eq, Ord, Num)

instance Monoid SummableInt where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

data Resources = Resources
  { _attack :: SummableInt
  , _money  :: SummableInt
  } deriving (Show, Generic, Eq)

instance Monoid Resources where
  mempty = Resources { _attack = mempty,       _money = mempty }
  mappend Resources { _attack = a1,      _money = m1 }
          Resources { _attack = a2,      _money = m2 } =
          Resources { _attack = a1 <> a2, _money = m1 <> m2 }

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

type CardMap = M.HashMap Location (S.Seq CardInPlay)

data GameState = WaitingForChoice T.Text | Preparing | Won | Lost T.Text deriving (Show, Generic, Eq)

data Player = Player
  { _resources :: Resources
  , _playerId :: PlayerId
  }
  deriving (Show, Generic, Eq)

data Effect =
  EffectNone |
  EffectMoney SummableInt |
  EffectAttack SummableInt |
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
  , _actionLog     :: S.Seq Action
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

data Condition =
  ConditionCostLTE SpecificCard Int deriving (Show, Generic)

data Action =
  ActionNone |
  ActionCombine Action Action |
  MoveCard SpecificCard Location MoveDestination |
  RevealCard SpecificCard Visibility |
  ApplyResources PlayerId Resources |
  ActionShuffle Location |
  ActionIf Condition Action Action |
  ActionHalt Action T.Text |
  ActionTagged T.Text Action |

  ActionLose T.Text |
  ActionPlayerTurn PlayerId |
  ActionStartTurn |
  ActionPrepareGame |
  ActionEndTurn

  deriving (Generic)

instance Show Action where
  show ActionNone = "None"
  show ActionStartTurn = "Turn Start"
  show ActionEndTurn = "Turn End"
  show (ActionPlayerTurn pid) = "Player turn: " <> show pid
  show (ActionLose s) = "Lose: " <> T.unpack s
  show (ActionCombine a b) = show a <> "\n" <> show b
  show (ActionShuffle location) = "Shuffle: " <> show location
  show (MoveCard specificCard to dest) = "Move: " <> show specificCard <> " to " <> show to <> "/" <> show dest
  show (RevealCard specificCard vis) = "Reveal: " <> show specificCard <> " = " <> show vis

  show _ = "Some Action"

instance Monoid Action where
  mempty = ActionNone
  mappend a b = ActionCombine a b

makeLenses ''Player
makeLenses ''CardInPlay
makeLenses ''Board
makeLenses ''Card
makeLenses ''Resources
makeLenses ''Game

instance Eq Card where
  a == b =
    view cardType a == view cardType b &&
    view templateId a == view templateId b

instance Eq CardInPlay where
  a == b = view cardId a == view cardId b

mkBoard :: Board
mkBoard = Board
  { _players = mempty
  , _boardState = Preparing
  , _cards = mempty
  , _currentAction = mempty
  , _rng = mkStdGen 0
  , _version = 1
  , _actionLog = mempty
  , _playerChoices = mempty
  }

mkPlayer :: PlayerId -> Player
mkPlayer pid = Player
  { _resources = mempty
  , _playerId  = pid
  }

-- Return a unique list of all cards in use on the board
cardDictionary :: Board -> [Card]
cardDictionary board =
    nub
  . toList
  . fmap (view cardTemplate)
  . mconcat
  . fmap (\l -> view (cardsAtLocation l) board)
  $ allLocations board

  where
    allLocations board =
      let playerIds = [0 .. (S.length . view players $ board) - 1] in

         [HQ, HeroDeck, VillainDeck, Escaped, Boss]
      <> allCityLocations
      <> concatMap allPlayerLocations playerIds

    allCityLocations = City <$> [0..4]
    allPlayerLocations playerId =
      PlayerLocation (PlayerId playerId) <$> [(minBound :: ScopedLocation)..]

cardName = lens getter setter
  where
    getter c@HeroCard{} = view heroName c
    getter c@EnemyCard{} = view enemyName c

    setter c@HeroCard{} x = set heroName x c
    setter c@EnemyCard{} x = set enemyName x c

cardType = lens getter setter
  where
    getter c@HeroCard{} = view (to (const "hero")) c
    getter c@EnemyCard{} = view (to (const "enemy")) c

    -- TODO: In theory should be able to define a Getter but I couldn't figure
    -- it out.
    setter = undefined

templateId :: Lens' Card T.Text
templateId = lens getter setter
  where
    getter c = view cardName c <> "/" <> view heroAbilityName c

    -- TODO: In theory should be able to define a Getter but I couldn't figure
    -- it out.
    setter = undefined

cardsAtLocation :: Location -> Lens' Board (S.Seq CardInPlay)
cardsAtLocation l = cards . at l . non mempty

playerResources :: PlayerId -> Traversal' Board Resources
playerResources (PlayerId id) = players . ix id . resources

isPlaying :: Board -> Bool
isPlaying board = case view boardState board of
                    (WaitingForChoice _) -> True
                    _                    -> False

extractMoney (EffectMoney n) = n
extractMoney _ = mempty

extractAttack (EffectAttack n) = n
extractAttack _ = mempty

extractDescription (EffectCustom d _) = d
extractDescription _ = ""

addChoice :: PlayerId -> PlayerChoice -> Board -> Board
addChoice playerId choice =
  over (playerChoices . at playerId . non mempty) (choice S.<|)

baseResource f = walk . view playEffect
  where
    walk (EffectCombine a b) = walk a <> walk b
    walk x = f x

isLost :: Board -> Bool
isLost board = f $ view boardState board
  where
    f (Lost _) = True
    f _        = False

playerDesc (PlayerId id) = "Player " <> showT id

