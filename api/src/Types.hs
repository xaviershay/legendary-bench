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

import Utils

data Effect =
  EffectNone |
  EffectMoney Int |
  EffectAttack Int |
  EffectCombine Effect Effect
  deriving (Show, Generic, Eq)

instance Monoid Effect where
  mempty = EffectNone
  mappend a b = EffectCombine a b

type SpecificCard = (Location, Int)
data MoveDestination = Front | Back | Sorted deriving (Show, Generic)

data PlayerAction =
  PlayCard SpecificCard |
  AttackCard SpecificCard |
  PurchaseCard SpecificCard |
  FinishTurn

  deriving (Show, Generic)

data Visibility = All | Owner | Hidden deriving (Show, Generic, Eq)

data Card = PlayerCard
  { name       :: T.Text
  , playEffect :: Effect
  }
  deriving (Show, Generic, Eq)

data CardInPlay = CardInPlay Card Visibility deriving (Show, Generic, Eq)

data Resources = Resources
  { _attack :: Int
  , _money  :: Int
  } deriving (Show, Generic)

instance Monoid Resources where
  mempty = Resources { _attack = 0,       _money = 0 }
  mappend (Resources { _attack = a1,      _money = m1 })
          (Resources { _attack = a2,      _money = m2 }) =
           Resources { _attack = a1 + a2, _money = m1 + m2 }

newtype PlayerId = PlayerId Int deriving (Show, Generic, Eq)

data Location = PlayerLocation PlayerId ScopedLocation | Boss deriving (Show, Generic, Eq)
data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory deriving (Show, Generic, Eq)

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

type CardMap = M.HashMap Location (S.Seq CardInPlay)

data GameState = Playing | Won | Lost T.Text deriving (Show, Generic)

data Player = Player
  { _resources :: Resources
  }
  deriving (Show, Generic)

makeLenses ''Player

data Board = Board
  { _players :: S.Seq Player
  , _cards   :: CardMap
  , _gameState   :: GameState
  }
  deriving (Show, Generic)

makeLenses ''Board

data Game = Game
  { board :: Board
  }
  deriving (Show, Generic)

data Action =
  ActionNone |
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


mkPlayerDeck = S.replicate 8 moneyCard <> S.replicate 4 attackCard

moneyCard = PlayerCard
  { name = "Money"
  , playEffect = EffectMoney 1
  }

attackCard = PlayerCard
  { name = "Attack"
  , playEffect = EffectAttack 1
  }

mkGame :: Game
mkGame = Game
  { board = play (PlayerId 0) 0 $ draw 6 (PlayerId 0) $ Board
     { _players = S.fromList [Player { _resources = mempty }]
    , _gameState = Playing
    , _cards = M.fromList
        [ (PlayerLocation (PlayerId 0) PlayerDeck, fmap hideCard mkPlayerDeck)
        ]
    }
  }

play :: PlayerId -> Int -> Board -> Board
play id i board =
  case lookupCard location board of
    Nothing -> lose ("No card at: " <> showT location) board
    Just c -> apply
                $ ActionSequence (revealAction <> moveAction) (resourcesAction c)
                $ board
  where
    location = (PlayerLocation id Hand, i)
    moveAction = MoveCard location (PlayerLocation id Played) Front
    revealAction = RevealCard location All
    resourcesAction c board = ApplyResources id (resourcesFrom c board)

resourcesFrom :: CardInPlay -> Board -> Resources
resourcesFrom (CardInPlay card _) board =
  Resources
    { _attack = calculateAttack (playEffect card) board
    , _money = calculateMoney (playEffect card) board
    }

lookupCard :: SpecificCard -> Board -> Maybe CardInPlay
lookupCard (location, i) board = preview (cards . at location . _Just . ix i) board

calculateAttack :: Effect -> Board -> Int
calculateAttack (EffectAttack n) _ = n
calculateAttack (EffectCombine a b) board = calculateAttack a board + calculateAttack b board
calculateAttack _ _ = 0

calculateMoney :: Effect -> Board -> Int
calculateMoney (EffectMoney n) _ = n
calculateMoney (EffectCombine a b) board = calculateMoney a board + calculateMoney b board
calculateMoney _ _ = 0

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
lose reason board = set gameState (Lost reason) board

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
  over (players . ix id . resources) (rs <>) board
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
