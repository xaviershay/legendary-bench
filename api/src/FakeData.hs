{-# LANGUAGE OverloadedStrings #-}

module FakeData where

import qualified Data.HashMap.Strict as M
import qualified Data.Sequence       as S
import           System.Random       (StdGen, mkStdGen)

import Types
import Evaluator
import Cards
import Utils
import GameMonad
import Action

mkGame :: StdGen -> Game
mkGame g = Game
  { _gameState =
    prepareBoard $ Board
     { _players = S.fromList
                    [ Player { _resources = mempty, _playerId = PlayerId 0 }
                    ]
     , _rng = g
     , _boardState = Preparing
     , _version = 1
     , _currentAction = mempty
     , _playerChoices = mempty
     , _actionLog     = mempty
     , _cards = M.fromList
        [ (PlayerLocation (PlayerId 0) PlayerDeck, fmap hideCard mkPlayerDeck)
        , (HQ, S.fromList [CardInPlay spideyCard All])
        , (HeroDeck, S.fromList [CardInPlay spideyCard Hidden])
        , (VillianDeck, S.fromList (replicate 30 (CardInPlay villianCard Hidden)))
        ]
    }
  }

mkPlayerDeck = S.replicate 1 spideyCard <> S.replicate 8 moneyCard <> S.replicate 4 attackCard

prepareBoard board = runGameMonad board (apply ActionPrepareGame)

hideCard card = CardInPlay card Hidden
