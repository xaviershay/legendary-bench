{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import qualified Data.HashMap.Strict as M
import           Data.Monoid         (mempty, (<>))
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           GHC.Generics
import Data.Hashable (Hashable)
import Control.Lens

data Effect =
  EffectNone |
  EffectMoney Int |
  EffectAttack Int |
  EffectCombine Effect Effect
  deriving (Show, Generic)

instance Monoid Effect where
  mempty = EffectNone
  mappend a b = EffectCombine a b

data Visibility = All | Owner | Hidden deriving (Show, Generic)

data Card = PlayerCard
  { name       :: T.Text
  , playAffect :: Effect
  }
  deriving (Show, Generic)

data CardInPlay = CardInPlay Card Visibility deriving (Show, Generic)

data Player = Player
  { attack :: Int
  , money  :: Int
  }
  deriving (Show, Generic)

newtype PlayerId = PlayerId Int deriving (Show, Generic, Eq)

data Location = PlayerLocation PlayerId ScopedLocation | Boss deriving (Show, Generic, Eq)
data ScopedLocation = Hand | Played | PlayerDeck | Discard | Victory deriving (Show, Generic, Eq)

instance Hashable PlayerId
instance Hashable ScopedLocation
instance Hashable Location

data Board = Board
  { _players :: V.Vector Player
  , _cards   :: M.HashMap Location [CardInPlay]
  }
  deriving (Show, Generic)

makeLenses ''Board

data Game = Game
  { board :: Board
  }
  deriving (Show, Generic)

mkPlayerDeck = replicate 8 moneyCard <> replicate 4 attackCard

moneyCard = PlayerCard
  { name = "Money"
  , playAffect = EffectMoney 1
  }

attackCard = PlayerCard
  { name = "Attack"
  , playAffect = EffectAttack 1
  }

mkGame :: Game
mkGame = Game
  { board = redact (PlayerId 0) $ draw 6 (PlayerId 0) $ Board
    { _players = V.fromList [Player { attack = 0, money = 0}]
    , _cards = M.fromList
        [ (PlayerLocation (PlayerId 0) PlayerDeck, map hideCard mkPlayerDeck)
        ]
    }
  }

redact :: PlayerId -> Board -> Board
redact id board = over cards (M.mapWithKey f) board
  where
    f (PlayerLocation owner _) cs =
      let desired = if owner == id then All else Hidden in

      map (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired (CardInPlay card Owner) = CardInPlay card desired
    transformOwned _ x = x

draw :: Int -> PlayerId -> Board -> Board
draw 0 _ board = board
draw n id board = let
  deckKey = PlayerLocation id PlayerDeck
  handKey = PlayerLocation id Hand
  deck = M.lookupDefault [] deckKey $ view cards board
  hand = M.lookupDefault [] handKey $ view cards board
  in

  let newCards = case deck of
                  [] -> undefined -- Need to shuffle in discard, lose game if still none
                  (x:xs) -> M.insert deckKey xs $
                            M.insert handKey (revealToOwner x:hand) $
                            (view cards board) in

  draw (n - 1) id $ set cards newCards board

hideCard card = CardInPlay card Hidden

revealToOwner (CardInPlay card _) = (CardInPlay card Owner)
