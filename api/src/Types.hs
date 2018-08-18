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
import           Data.List            (nub)
import qualified Data.Sequence        as S
import qualified Data.Set             as Set
import           Data.String          (IsString, fromString)
import qualified Data.Text            as T
import           GHC.Generics         hiding (to, UInt)
import           System.Random        (StdGen, mkStdGen)

import Utils

data GameMonadState = GameMonadState
  { _board        :: Board
  , _currentCard  :: Maybe SpecificCard
  } deriving (Show, Generic)

type GameHalt = (Board, Action)
type GameMonad a = (ExceptT GameHalt (ReaderT GameMonadState (WriterT (S.Seq Action) Identity))) a

data SpecificCard =
    CardByIndex (Location, Int)
  -- Location tehnically not needed here, but don't currently keep an index of
  -- where cards are so this helps find it, given we always have the location
  -- when specifying a card anyways (at least for now).
  | CardById (Location, CardId)
  deriving (Show, Generic)

instance Eq SpecificCard where
  (CardById (_, x)) == (CardById (_, y)) = x == y
  x == y = error "Tried to compare CardByIndex. This _may_ be legitimate, but erroring to catch any cases of it coz probably should be CardById"

data MoveDestination = Top | Front | Back | LocationIndex Int deriving (Show, Generic, Eq)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq, Bounded, Enum)

data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory | Working
  deriving (Show, Generic, Eq, Enum, Bounded, Read)

newtype HeroType = HeroType T.Text
  deriving (Show, Generic, Eq, Monoid)

newtype HeroTeam = HeroTeam T.Text
  deriving (Show, Generic, Eq, Monoid)

data Location = PlayerLocation PlayerId ScopedLocation
  | HQ
  | KO
  | MastermindDeck
  | HeroDeck
  | VillainDeck
  | City Int
  | Escaped
  | Boss
  | BystanderDeck
  | WoundDeck
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

instance Monoid UExpr where
  mempty = UConst UNone
  mappend a b = USequence [a, b]

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
 | UBoardFunc Bindings UExpr
 | UAction Action
 | USpecificCard SpecificCard
 | UCardTemplate Card
 | UPlayerId PlayerId
 | UList [UExpr]
 | UError Name
 deriving (Show)

showUValue (UError e) = e
showUValue x = showT x

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
    | otherwise  = WVar . T.pack $ x

type EvalMonad a = (ExceptT T.Text (ReaderT Int (State UEnv))) a
type BuiltIn = (MType, EvalMonad UExpr)

data InferError =
    CannotUnify MType MType
  | OccursCheckFailed Name MType
  | UnknownIdentifier Name
  | NestedInferError T.Text
  deriving (Eq)

instance Show InferError where
  show (CannotUnify a b) = T.unpack $ "Cannot unify:\n  " <> showType a <> "\n  " <> showType b
  show (UnknownIdentifier n) = "Unknown identifier: " <> T.unpack n
  show (OccursCheckFailed a b) = T.unpack $ "Occurs check failed:\n  " <> a <> "\n  " <> showType b
  show (NestedInferError x) = T.unpack x

showType (WVar x) = x
showType (WConst x) = x
showType (WFun x y) = maybeBracket (isFun x) (showType x) <> " -> " <> showType y
  where
    isFun WFun{} = True
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

newtype JoinableText = JoinableText T.Text
  deriving (Show, Generic)

instance Monoid JoinableText where
  mempty = JoinableText mempty
  mappend (JoinableText a) (JoinableText b) = JoinableText (a <> " " <> b)

type LabeledExpr = (T.Text, UExpr)
type LabeledAction = (JoinableText, Action)

extractLabel = fst
extractCode = snd

mkLabeledExpr = (,)

-- A SummableInt that has a base value, and potentially an expression that can
-- be dynamically evaluated to modify that value. The expression is stored as a
-- Maybe rather than an empty expression so that it can be displayed
-- differently in human-readable form.
data ModifiableInt = ModifiableInt SummableInt (Maybe UExpr)
  deriving (Show, Generic)

mkModifiableInt = ModifiableInt

instance Monoid ModifiableInt where
  mempty = ModifiableInt mempty Nothing
  mappend (ModifiableInt b1 m1) (ModifiableInt b2 m2) = error "mappend unimplemented for ModifiableInt"

data Card = HeroCard
  { _heroName   :: T.Text
  , _heroAbilityName :: T.Text
  , _heroType :: HeroType
  , _heroTeam :: HeroTeam
  , _heroDescription :: T.Text
  , _playCode :: Action
  , _playGuard :: UExpr
  , _discardEffect :: UExpr
  , _woundEffect :: UExpr
  , _heroCost   :: SummableInt
  , _heroStartingNumber :: SummableInt
  , _recruitPip :: Maybe T.Text
  , _attackPip :: Maybe T.Text
  } | EnemyCard
  { _enemyName :: T.Text
  , _enemyTribe :: T.Text
  , _enemyStartingNumber :: SummableInt
  , _enemyAttack :: ModifiableInt
  , _enemyVP :: ModifiableInt
  , _enemyDescription :: T.Text
  , _fightCode :: LabeledAction
  , _fightGuard :: UExpr
  , _escapeCode :: Maybe LabeledAction
  , _ambushCode :: Maybe LabeledAction
  }
  | MastermindCard
  { _mmName :: T.Text
  , _mmStrikeCode :: LabeledExpr
  , _mmAlwaysLeads :: T.Text
  , _mmAttack :: ModifiableInt
  , _mmVP :: ModifiableInt
  }
  | MastermindTacticCard
  { _mmtFightCode :: LabeledAction
  , _mmtName :: T.Text
  , _mmtAbilityName :: T.Text
  -- Will just be copied from MM, but denormalized to avoid having to reference
  -- another card to calculate these.
  , _mmtAttack :: ModifiableInt
  , _mmtVP :: ModifiableInt
  }
  | TwistCard
  | MasterStrikeCard
  | BystanderCard
  | WoundCard

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

data GameState = WaitingForChoice T.Text | Preparing | Won T.Text | Lost T.Text deriving (Show, Generic, Eq)

instance Monoid GameState where
  mempty = Preparing
  mappend _ (Won x) = Won x
  mappend (Won x) _ = Won x
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
  , _postDrawActions :: M.HashMap PlayerId Action
  , _actionLog     :: S.Seq Action
  , _playerChoices :: M.HashMap PlayerId (S.Seq PlayerChoice)
  , _turnStack     :: S.Seq PlayerId
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
  ChooseBool Bool |
  ChoosePass |
  ChooseEndTurn
  deriving (Show, Generic, Eq)

data Condition =
  ConditionCostLTE SpecificCard Int deriving (Show, Generic, Eq)

data Action =
  ActionNone |
  ActionCombine Action Action |
  ActionAllowFail Action |
  ActionVisibility SpecificCard Visibility |
  ActionHide SpecificCard |
  ActionMove SpecificCard Location MoveDestination |
  ApplyResources PlayerId Resources |
  ActionShuffle Location |
  ActionOptional Action Action Action |
  ActionHalt Action T.Text |
  ActionTagged T.Text Action |
  ActionTrace T.Text |
  ActionConcurrent [Action] |
  ActionChooseCard PlayerId T.Text [SpecificCard] UExpr (Maybe Action) |
  ActionChooseYesNo PlayerId T.Text Action Action |
  ActionEndStep Action |

  ActionAttack PlayerId SummableInt |
  ActionRecruit PlayerId SummableInt |
  ActionRescueBystander PlayerId SummableInt |
  ActionCaptureBystander SpecificCard SummableInt |
  ActionDraw PlayerId SummableInt|
  ActionKO SpecificCard |
  ActionDiscardCard SpecificCard |
  ActionDefeat PlayerId SpecificCard |
  ActionGainWound PlayerId Location SummableInt |
  ActionGain PlayerId SpecificCard |

  ActionLose T.Text |
  ActionWin T.Text |
  ActionPlayerTurn PlayerId |
  ActionStartTurn |
  ActionPrepareGame |
  ActionEndTurn |

  -- TODO: Think through these cases more
  ActionKOHero |
  ActionDiscard PlayerId |
  ActionEval Bindings UExpr

  deriving (Generic, Show)

instance Monoid Action where
  mempty = ActionNone
  mappend = ActionCombine

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
  , _postDrawActions = mempty
  , _turnStack = mempty
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

         [HQ, HeroDeck, VillainDeck, Escaped, Boss, BystanderDeck, WoundDeck, MastermindDeck]
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
    getter c@WoundCard = view (to (const "Wound")) c
    getter c@MasterStrikeCard = view (to (const "Master Strike")) c
    getter c@TwistCard = view (to (const "Twist")) c
    getter c@MastermindTacticCard{} = view mmtName c
    getter c@MastermindCard{} = view mmName c

    setter c@HeroCard{} x = set heroName x c
    setter c@EnemyCard{} x = set enemyName x c
    setter c _ = c

cardType = lens getter setter
  where
    getter c@HeroCard{} = "hero"
    getter c@EnemyCard{} = "enemy"
    getter c@BystanderCard = "bystander"
    getter c@WoundCard = "wound"
    getter c@MastermindTacticCard{} = "mastermind-tactic"
    getter c@MastermindCard{} = "mastermind"
    getter c@MasterStrikeCard = "masterstrike"
    getter c@TwistCard = "twist"

    setter = const

templateId :: Lens' Card T.Text
templateId = lens getter setter
  where
    getter c@HeroCard{} = "Hero" <> "/" <> view cardName c <> "/" <> view heroAbilityName c
    getter c@EnemyCard{} = "Enemy" <> "/" <> view enemyTribe c <> "/" <> view cardName c
    getter c@MastermindCard{} = "Mastermind" <> "/" <> view cardName c
    getter c@MastermindTacticCard{} = "MastermindTactic" <> "/" <> view cardName c <> "/" <> view mmtAbilityName c
    getter c = "Other" <> "/" <> view cardName c

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

extractDescription (ActionTagged d _) = d
extractDescription _ = ""

addChoice :: PlayerId -> PlayerChoice -> Board -> Board
addChoice playerId choice =
  over (playerChoices . at playerId . non mempty) (choice S.<|)

isLost :: Board -> Bool
isLost board = f $ view boardState board
  where
    f (Lost _) = True
    f _        = False

playerDesc (PlayerId id) = "Player " <> showT id
playerDesc CurrentPlayer = "Current player"

emptyEnv = UEnv { _envVariables = mempty, _envBoard = Nothing, _envCards = mempty, _envBuiltIn = mempty, _envBuiltInDefs = mempty }
extendEnv :: M.HashMap Name UExpr -> UEnv -> UEnv
extendEnv newVars = over envVariables (\x -> newVars `M.union` x)

infixr 8 ~>
(~>) :: MType -> MType -> MType
a ~> b = WFun a b

specificCard :: Location -> Int -> SpecificCard
specificCard x y = CardByIndex (x, y)

specificCardByIndex :: Location -> Int -> SpecificCard
specificCardByIndex x y = CardByIndex (x, y)

cardByIndex :: Location -> Int -> SpecificCard
cardByIndex x y = CardByIndex (x, y)

cardById :: Location -> CardId -> SpecificCard
cardById x y = CardById (x, y)

cardLocation (CardByIndex (l, _)) = l
cardLocation (CardById (l, _)) = l

cardAtLocation (CardByIndex (location, index)) = cardsAtLocation location . ix index
cardAtLocation (CardById (location, cid))      = cardsAtLocation location . folded . filtered (\card -> view cardId card == cid)
