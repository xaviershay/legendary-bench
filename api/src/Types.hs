{-# LANGUAGE DeriveGeneric     #-} {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State  (State)
import           Control.Monad.Writer (WriterT)
import           Data.Char            (isUpper)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as M
import           Data.List            (intercalate, nub)
import qualified Data.Sequence        as S
import qualified Data.Set             as Set
import           Data.String          (IsString, fromString)
import qualified Data.Text            as T
import           GHC.Generics         hiding (to)
import           System.Random        (StdGen, mkStdGen)

import Debug.Trace

import Utils

data GameMonadState = GameMonadState
  { _board        :: Board
  }

type GameHalt = (Board, Action)
type GameMonad a = (ExceptT GameHalt (ReaderT GameMonadState (WriterT (S.Seq Action) Identity))) a

type SpecificCard = (Location, Int)
data MoveDestination = Top | Front | Back | LocationIndex Int deriving (Show, Generic, Eq)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq, Bounded, Enum)

data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory
  deriving (Show, Generic, Eq, Enum, Bounded, Read)

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
  | BystanderDeck
  deriving (Show, Generic, Eq)

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
  deriving (Show)

data UEnv = UEnv
  { _envVariables :: M.HashMap Name UExpr
  , _envBoard :: Maybe Board
  , _envCards :: S.Seq Card
  -- TODO: Maybe don't need both here, abstract away accessors so can simplify
  , _envBuiltIn :: M.HashMap Name UExpr
  , _envBuiltInDefs :: M.HashMap Name BuiltInDef
  }

instance Show UEnv where
  -- Important not to naively dump out envVariables, since they contain cyclic
  -- references to the env...
  show x = "[UEnv ...]"

instance Eq UEnv where
  a == b = True

type Bindings = M.HashMap Name UExpr
data UFuncData = UFuncData
  { _fnBindings :: Bindings
  , _fnFreeVars :: Set.Set Name
  , _fnArgName  :: Name
  , _fnBody     :: UExpr
  } deriving (Show)

data UValue =
   UNone
 | ULocation Location
 | UInt SummableInt
 | UString T.Text
 | UBool Bool
 | UFunc UFuncData
 | UBoardFunc UEnv UExpr
 | UAction Action
 | USpecificCard SpecificCard
 | UCardTemplate Card
 | UPlayerId PlayerId
 | UList [UExpr]
 | UError Name
 deriving (Show)

data MType =
    WVar Name
  | WConst Name
  | WFun MType MType
  | WBoardF MType
  | WList MType

  deriving (Eq, Show)

data PType = Forall (Set.Set Name) MType deriving (Show)

instance IsString MType where
  fromString x@(h:_) 
    | isUpper h  = WConst . T.pack $ x
    | True       = WVar . T.pack $ x

type EvalMonad a = (ExceptT T.Text (ReaderT Int (State UEnv))) a
type BuiltIn = (MType, EvalMonad UExpr)

data InferError =
    CannotUnify MType MType
  | OccursCheckFailed Name MType
  | UnknownIdentifier Name
  deriving (Eq)

instance Show InferError where
  show (CannotUnify a b) = T.unpack $ "Cannot unify:\n  " <> showType a <> "\n  " <> showType b
  show (UnknownIdentifier n) = "Unknown identifier: " <> T.unpack n

showType (WVar x) = x
showType (WConst x) = x
showType (WFun x y) = maybeBracket (isFun x) (showType x) <> " -> " <> showType y
  where
    isFun (WFun{}) = True
    isFun _ = False
    maybeBracket cond x = if cond then "(" <> x <> ")" else x
showType (WList x) = "[" <> showType x <> "]"
showType (WBoardF x) = "@:" <> showType x


data BuiltInDef = BuiltInDef
  { _builtInName :: Name
  , _builtInType :: PType
  , _builtInFn :: EvalMonad UExpr
  }

data PlayerId = CurrentPlayer | PlayerId Int deriving (Show, Generic, Eq)

data Card = HeroCard
  { _heroName   :: T.Text
  , _heroAbilityName :: T.Text
  , _heroType :: HeroType
  , _heroTeam :: HeroTeam
  , _heroDescription :: T.Text
  , _playEffect :: Action
  , _playCode :: UExpr
  , _heroCost   :: SummableInt
  , _heroStartingNumber :: SummableInt
  } | EnemyCard
  { _enemyName :: T.Text
  , _baseHealth :: SummableInt
  } | BystanderCard

  deriving (Show, Generic)

newtype CardId = CardId Int deriving (Show, Generic, Eq)

data CardInPlay = CardInPlay
  { _cardId :: CardId
  , _cardVisibility :: Visibility
  , _cardTemplate :: Card
  } deriving (Show, Generic)

newtype SummableInt = Sum Int deriving (Show, Generic, Eq, Ord, Num)

toInt (Sum x) = x

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

instance Monoid GameState where
  mempty = Preparing
  mappend _ Won = Won
  mappend Won _ = Won
  mappend (Lost x) _ = Lost x
  mappend _ (Lost x) = Lost x
  mappend a Preparing = a
  mappend Preparing b = b
  mappend (WaitingForChoice x) (WaitingForChoice y) = WaitingForChoice $ x <> ", " <> y

data Player = Player
  { _resources :: Resources
  , _playerId :: PlayerId
  }
  deriving (Show, Generic, Eq)

data Effect =
  EffectNone |
  EffectMoney SummableInt |
  EffectAttack SummableInt |
  EffectCustom T.Text Action |
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

showTerms :: Show a => T.Text -> [a] -> String
showTerms t args = T.unpack $ t <> " (" <> (T.intercalate ", " . map showT $ args) <> ")"

showTerms2 t (x, y) = showTerms t [show x, show y]
showTerms3 t (x, y, z) = showTerms t [show x, show y, show z]

data PlayerChoice =
  ChooseCard SpecificCard |
  ChoosePass |
  ChooseEndTurn
  deriving (Show, Generic, Eq)

data Condition =
  ConditionCostLTE SpecificCard Int deriving (Show, Generic, Eq)

data Action =
  ActionNone |
  ActionCombine Action Action |
  ActionAllowFail Action |
  ActionReveal SpecificCard |
  ActionHide SpecificCard |
  ActionMove SpecificCard Location MoveDestination |
  ApplyResources PlayerId Resources |
  ActionShuffle Location |
  ActionOptional Action Action Action |
  ActionHalt Action T.Text |
  ActionTagged T.Text Action |
  ActionTrace T.Text |
  ActionConcurrent [Action] |
  ActionChooseCard T.Text [SpecificCard] UExpr Action |

  ActionAttack PlayerId SummableInt |
  ActionRecruit PlayerId SummableInt |
  ActionRescueBystander PlayerId SummableInt |
  ActionDraw PlayerId SummableInt|
  ActionKO SpecificCard |

  ActionLose T.Text |
  ActionPlayerTurn PlayerId |
  ActionStartTurn |
  ActionPrepareGame |
  ActionEndTurn |

  -- TODO: Think through these cases more
  ActionKOHero |
  ActionDiscard PlayerId

  deriving (Generic, Show)

instance Monoid Action where
  mempty = ActionNone
  mappend a b = ActionCombine a b

makeLenses ''Player
makeLenses ''CardInPlay
makeLenses ''Board
makeLenses ''Card
makeLenses ''Resources
makeLenses ''Game
makeLenses ''UFuncData
makeLenses ''BuiltInDef
makeLenses ''UEnv

instance Eq Card where
  a == b =
    view cardType a == view cardType b &&
    view templateId a == view templateId b

instance Eq CardInPlay where
  a == b = view cardId a == view cardId b

deriving instance Eq UExpr
deriving instance Eq UValue
deriving instance Eq UFuncData
deriving instance Eq Action

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

    allPlayerLocations playerId =
      PlayerLocation (PlayerId playerId) <$> [(minBound :: ScopedLocation)..]

allCityLocations = City <$> [0..4]

cardName = lens getter setter
  where
    getter c@HeroCard{} = view heroName c
    getter c@EnemyCard{} = view enemyName c
    getter c@BystanderCard = view (to (const "Bystander")) c

    setter c@HeroCard{} x = set heroName x c
    setter c@EnemyCard{} x = set enemyName x c
    setter c@BystanderCard x = c

cardType = lens getter setter
  where
    getter c@HeroCard{} = "hero"
    getter c@EnemyCard{} = "enemy"
    getter c@BystanderCard = "bystander"

    setter c = const c

templateId :: Lens' Card T.Text
templateId = lens getter setter
  where
    getter c = view cardName c <> "/" <> view heroAbilityName c

    -- TODO: In theory should be able to define a Getter but I couldn't figure
    -- it out.
    setter = undefined

cardsAtLocation :: Location -> Lens' Board (S.Seq CardInPlay)
cardsAtLocation (PlayerLocation CurrentPlayer _) = error "Location must be resolved"
cardsAtLocation l = cards . at l . non mempty

playerResources :: PlayerId -> Traversal' Board Resources
playerResources (PlayerId id) = players . ix id . resources

isPlaying :: Board -> Bool
isPlaying board = case view boardState board of
                    (WaitingForChoice _) -> True
                    _                    -> False

extractMoney (ActionRecruit _ n) = n
extractMoney _ = mempty

extractAttack (ActionAttack _ n) = n
extractAttack _ = mempty

extractDescription (ActionTagged d _) = d
extractDescription _ = ""

addChoice :: PlayerId -> PlayerChoice -> Board -> Board
addChoice playerId choice =
  over (playerChoices . at playerId . non mempty) (choice S.<|)

baseResource f = walk . view playEffect
  where
    walk (ActionCombine a b) = walk a <> walk b
    walk x = f x

isLost :: Board -> Bool
isLost board = f $ view boardState board
  where
    f (Lost _) = True
    f _        = False

playerDesc (PlayerId id) = "Player " <> showT id
playerDesc CurrentPlayer = "Current player"

emptyEnv = UEnv { _envVariables = mempty, _envBoard = Nothing, _envCards = mempty, _envBuiltIn = mempty }
extendEnv :: M.HashMap Name UExpr -> UEnv -> UEnv
extendEnv newVars env = over envVariables (\x -> newVars `M.union` x) env

infixr 8 ~>
(~>) :: MType -> MType -> MType
a ~> b = WFun a b

specificCard :: Location -> Int -> SpecificCard
specificCard x y = (x, y)
