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
  EffectCustom T.Text (PlayerId -> Board -> Action) |
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
  ActionSequence Action (Board -> Action) |
  MoveCard SpecificCard Location MoveDestination |
  RevealCard SpecificCard Visibility |
  ApplyResources PlayerId Resources

  deriving (Generic)

instance Show Action where
  show ActionNone = "None"

instance Monoid Action where
  mempty = ActionNone
  mappend a b = ActionSequence a (const b)

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

data GameMonadState = GameMonadState
  { _activePlayer :: PlayerId
  , _board        :: Board
  }

type GameMonad = Reader GameMonadState

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
drawAction id 1 = let location = (PlayerLocation id PlayerDeck, 0) in
     RevealCard location All
  <> MoveCard location (PlayerLocation id Hand) Front

spideyAction :: PlayerId -> Board -> Action
spideyAction id board =
  let location = (PlayerLocation id PlayerDeck, 0) in

  RevealCard location All <>
    case lookupCard location board of
       Nothing -> ActionLose "No cards left in deck" -- TODO: Shuffle in discard
       Just c -> if cardCost c <= 2 then
                   drawAction id 1
                 else
                   ActionNone

mkGame :: Game
mkGame = Game
  { _gameState =
   -- purchase (PlayerId 0) 0 $
   -- play (PlayerId 0) 0 $
    play (PlayerId 0) 5 $
    draw 6 (PlayerId 0) $ Board
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

runGameMonad :: PlayerId -> Board -> GameMonad a -> a
runGameMonad id board m = runReader m
  (GameMonadState { _activePlayer = id, _board = board })

playAction :: PlayerId -> CardInPlay -> Board -> Action
playAction id (CardInPlay card _) board = runGameMonad id board $
  effectAction (view playEffect card)

applyResourcesAction :: Resources -> GameMonad Action
applyResourcesAction rs = do
  player <- currentPlayer

  return $ ApplyResources player rs

effectAction :: Effect -> GameMonad Action
effectAction (EffectMoney n) = applyResourcesAction (set money n mempty)
effectAction (EffectAttack n) = applyResourcesAction (set attack n mempty)

effectAction (EffectNone) = return ActionNone
effectAction (EffectCustom _ f) = do
  player <- currentPlayer
  b <- currentBoard

  return $ f player b

effectAction (EffectCombine a b) = do
  x <- effectAction a
  y <- effectAction b

  return $ x <> y

translatePlayerAction :: PlayerId -> PlayerAction -> Board -> Action
translatePlayerAction id (PlayCard i) board =
  let location = (PlayerLocation id Hand, i) in

  case lookupCard location board of
    Nothing -> ActionLose ("No card at: " <> showT location)
    Just c ->
         RevealCard location All
      <> MoveCard location (PlayerLocation id Played) Front
      <> playAction id c board

translatePlayerAction id (PurchaseCard i) board =
  let location = (HQ, i) in

  case lookupCard location board of
    Nothing -> ActionLose ("No card to purchase: " <> showT location)
    Just c ->
         MoveCard location (PlayerLocation id Discard) Front
      <> ApplyResources id (mempty { _money = (-(cardCost c))})
      <> RevealCard (HeroDeck, 0) All
      <> MoveCard (HeroDeck, 0) HQ (LocationIndex i)

play :: PlayerId -> Int -> Board -> Board
play id i board = apply (translatePlayerAction id (PlayCard i) board) board

purchase :: PlayerId -> Int -> Board -> Board
purchase id i board = apply (translatePlayerAction id (PurchaseCard i) board) board

lookupCard :: SpecificCard -> Board -> Maybe CardInPlay
lookupCard (location, i) board = preview (cards . at location . _Just . ix i) board

redact :: PlayerId -> Board -> Board
redact id board = over cards (M.mapWithKey f) board
  where
    f (PlayerLocation owner _) cs =
      let desired = if owner == id then All else Hidden in

      fmap (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired (CardInPlay card Owner) = CardInPlay card desired
    transformOwned _ x = x

lose :: T.Text -> Board -> Board
lose reason board = set boardState (Lost reason) board

invalidResources :: Resources -> Bool
invalidResources r = (view money r < 0) || (view attack r < 0)

apply :: Action -> Board -> Board
apply (MoveCard specificCard@(location, i) to dest) board =
  -- "Pop" card from source
  case preview (cards . at location . _Just . ix i) board of
    Nothing -> (lose $ "Card does not exist: " <> showT specificCard) board
    Just card ->
      over (cards . at location) (fmap (S.deleteAt i)) $
      over (cards . at to . non S.Empty) (card <|) $
      board
apply (RevealCard (location, i) v) board =
  over (cards . at location . _Just . ix i) (setVisibility v) board
apply (ApplyResources (PlayerId id) rs) board =
  let board' = over (players . ix id . resources) (rs <>) board in

  if invalidResources (view (players . ix id . resources) board') then
    lose "Not enough resources" board'
  else
    board'

apply (ActionNone) board = board
apply (ActionSequence a f) board =
  let board' = apply a board in

  apply (f board') board'

apply action board = (lose $ "Don't know how to apply: " <> showT action) board

  -- Add card to destination
draw :: Int -> PlayerId -> Board -> Board
draw 0 _ board = board
draw n id board = let
  deckKey = PlayerLocation id PlayerDeck
  handKey = PlayerLocation id Hand
  deck = M.lookupDefault mempty deckKey $ view cards board
  hand = M.lookupDefault mempty handKey $ view cards board
  in

  let newCards = case deck of
                  S.Empty -> undefined -- Need to shuffle in discard, lose game if still none
                  (x S.:<| xs) -> M.insert deckKey xs $
                                  M.insert handKey (revealToOwner x <| hand) $
                                  (view cards board) in

  draw (n - 1) id $ set cards newCards board

hideCard card = CardInPlay card Hidden

setVisibility :: Visibility -> CardInPlay -> CardInPlay
setVisibility v (CardInPlay card _) = CardInPlay card v

revealToOwner = setVisibility Owner
