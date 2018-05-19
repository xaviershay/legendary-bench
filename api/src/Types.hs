{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import qualified Data.HashMap.Strict as M
import           Data.Monoid         (mempty, (<>))
import qualified Data.Text           as T
import qualified Data.Sequence       as S
import           GHC.Generics
import Data.Hashable (Hashable)
import Control.Lens
import Control.Monad.Reader

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
  FinishTurn

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
  } deriving (Show, Generic)

instance Monoid Resources where
  mempty = Resources { _attack = 0,       _money = 0 }
  mappend (Resources { _attack = a1,      _money = m1 })
          (Resources { _attack = a2,      _money = m2 }) =
           Resources { _attack = a1 + a2, _money = m1 + m2 }

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

type CardMap = M.HashMap Location (S.Seq CardInPlay)

data GameState = Playing | Won | Lost T.Text deriving (Show, Generic)

data Player = Player
  { _resources :: Resources
  }
  deriving (Show, Generic)

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
  ApplyResources PlayerId Resources

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

-- This instance is used for anything substantial, it's just needed for some
-- lens derivation (which we ultimately don't rely on)
instance Eq Card where
  (a@HeroCard{}) == (b@HeroCard{}) = view heroName a == view heroName b
  (a@EnemyCard{}) == (b@EnemyCard{}) = view enemyName a == view enemyName b

instance Eq CardInPlay

mkPlayerDeck = S.replicate 1 spideyCard <> S.replicate 8 moneyCard <> S.replicate 4 attackCard

moneyCard = HeroCard
  { _heroName = "Money"
  , _playEffect = EffectMoney 1
  , _cost = 0
  }

attackCard = HeroCard
  { _heroName = "Attack"
  , _playEffect = EffectAttack 1
  , _cost = 0
  }

spideyCard = HeroCard
  { _heroName = "Spiderman"
  , _playEffect =    EffectMoney 1
                  <> EffectCustom "Reveal top card of deck, if costs less than two then draw it" spideyAction
  , _cost = 2
  }

drawAction :: PlayerId -> Int -> Action
drawAction playerId n =
   mconcat . replicate n $
     revealAndMove
       (PlayerLocation playerId PlayerDeck, 0)
       (PlayerLocation playerId Hand)
       Front

spideyAction :: GameMonad Action
spideyAction = do
  playerId <- currentPlayer

  let location = (PlayerLocation playerId PlayerDeck, 0)

  card <- lookupCard location

  return $ RevealCard location All <>
    case card of
       Nothing -> ActionLose "No cards left in deck" -- TODO: Shuffle in discard
       Just c -> if cardCost c <= 2 then
                   drawAction playerId 1
                 else
                   ActionNone

mkGame :: Game
mkGame = Game
  { _gameState =
   -- purchase (PlayerId 0) 0 $
   -- play (PlayerId 0) 0 $
    play (PlayerId 0) 5 $
    draw (PlayerId 0) 6 $ Board
     { _players = S.fromList [Player { _resources = mempty }]
    , _boardState = Playing
    , _cards = M.fromList
        [ (PlayerLocation (PlayerId 0) PlayerDeck, fmap hideCard mkPlayerDeck)
        , (HQ, S.fromList [CardInPlay spideyCard All])
        , (HeroDeck, S.fromList [CardInPlay spideyCard Hidden])
        ]
    }
  }

cardCost :: CardInPlay -> Int
cardCost (CardInPlay (HeroCard { _cost = c }) _) = c
cardCost _ = 0

currentPlayer :: GameMonad PlayerId
currentPlayer = do
  state <- ask

  return $ _activePlayer state

currentBoard :: GameMonad Board
currentBoard = do
  state <- ask

  return $ _board state

withBoard :: Board -> GameMonad a -> GameMonad a
withBoard board m = do
  playerId <- currentPlayer

  let state = GameMonadState { _activePlayer = playerId, _board = board}

  return $ runReader m state

runGameMonad :: PlayerId -> Board -> GameMonad a -> a
runGameMonad id board m = runReader m
  (GameMonadState { _activePlayer = id, _board = board })

playAction :: CardInPlay -> GameMonad Action
playAction (CardInPlay card _) = effectAction (view playEffect card)

applyResourcesAction :: Resources -> GameMonad Action
applyResourcesAction rs = do
  player <- currentPlayer

  return $ ApplyResources player rs

effectAction :: Effect -> GameMonad Action
effectAction (EffectMoney n) = applyResourcesAction (set money n mempty)
effectAction (EffectAttack n) = applyResourcesAction (set attack n mempty)
effectAction (EffectNone) = return ActionNone
effectAction (EffectCustom _ f) = f
effectAction (EffectCombine a b) = do
  x <- effectAction a
  y <- effectAction b

  return $ x <> y

translatePlayerAction :: PlayerAction -> GameMonad Action
translatePlayerAction (PlayCard i) = do
  playerId <- currentPlayer

  let location = (PlayerLocation playerId Hand, i)

  card <- lookupCard location

  case card of
    Nothing -> return $ ActionLose ("No card at: " <> showT location)
    Just c -> do
      cardEffect <- playAction c

      return $
           revealAndMove location (PlayerLocation playerId Played) Front
        <> cardEffect

translatePlayerAction (PurchaseCard i) = do
  let location = (HQ, i)

  playerId <- currentPlayer
  card <- lookupCard location

  return $ case card of
    Nothing -> ActionLose ("No card to purchase: " <> showT location)
    Just c ->
         MoveCard location (PlayerLocation playerId Discard) Front
      <> ApplyResources playerId (mempty { _money = (-(cardCost c))})
      <> revealAndMove (HeroDeck, 0) HQ (LocationIndex i)

revealAndMove source destination spot =
     RevealCard source All
  <> MoveCard source destination spot

play :: PlayerId -> Int -> Board -> Board
play id i board = (runGameMonad id board $ translatePlayerAction (PlayCard i) >>= apply)

purchase :: PlayerId -> Int -> Board -> Board
purchase id i board = (runGameMonad id board $ translatePlayerAction (PurchaseCard i) >>= apply)

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) =
  preview (cards . at location . _Just . ix i) <$> currentBoard

redact :: PlayerId -> Board -> Board
redact id board = over cards (M.mapWithKey f) board
  where
    f (PlayerLocation owner _) cs =
      let desired = if owner == id then All else Hidden in

      fmap (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired (CardInPlay card Owner) = CardInPlay card desired
    transformOwned _ x = x

lose :: T.Text -> GameMonad Board
lose reason = set boardState (Lost reason) <$> currentBoard

invalidResources :: Resources -> Bool
invalidResources r = (view money r < 0) || (view attack r < 0)

apply :: Action -> GameMonad Board
apply (MoveCard specificCard@(location, i) to dest) = do
  board <- currentBoard
  -- "Pop" card from source
  case preview (cards . at location . _Just . ix i) board of
    Nothing -> (lose $ "Card does not exist: " <> showT specificCard)
    Just card -> return $
      over (cards . at location) (fmap (S.deleteAt i)) $
      over (cards . at to . non S.Empty) (card <|) $
      board
apply (RevealCard (location, i) v) =
  over (cards . at location . _Just . ix i) (setVisibility v) <$> currentBoard
apply (ApplyResources (PlayerId id) rs) = do
  board <- currentBoard

  let board' = over (players . ix id . resources) (rs <>) board

  if invalidResources (view (players . ix id . resources) board') then
    lose "Not enough resources"
  else
    return board'

apply (ActionNone) = currentBoard
apply (ActionSequence a m) = do
  board' <- apply a

  withBoard board' $ m >>= apply

apply action = lose $ "Don't know how to apply: " <> showT action

  -- Add card to destination
draw :: PlayerId -> Int -> Board -> Board
draw playerId n board =
  runGameMonad playerId board (apply $ drawAction playerId n)

hideCard card = CardInPlay card Hidden

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility v (CardInPlay card _) = CardInPlay card v
