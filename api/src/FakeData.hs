module FakeData where

import qualified Data.HashMap.Strict as M
import qualified Data.Sequence       as S
import           System.Random       (mkStdGen)

import Types
import Evaluator
import Cards
import Utils
import GameMonad
import Action

mkGame :: Game
mkGame = Game
  { _gameState =
    draw (PlayerId 0) 6 $ Board
     { _players = S.fromList [Player { _resources = mempty, _playerId = (PlayerId 0) }]
     , _rng = mkStdGen 0
     , _boardState = Playing
     , _version = 1
     , _currentAction = ActionPlayerTurn (PlayerId 0)
     , _playerChoices = mempty
     , _cards = M.fromList
        [ (PlayerLocation (PlayerId 0) PlayerDeck, fmap hideCard mkPlayerDeck)
        , (HQ, S.fromList [CardInPlay spideyCard All])
        , (HeroDeck, S.fromList [CardInPlay spideyCard Hidden])
        , (VillianDeck, S.fromList (replicate 30 (CardInPlay villianCard Hidden)))
        ]
    }
  }

mkPlayerDeck = S.replicate 1 spideyCard <> S.replicate 8 moneyCard <> S.replicate 4 attackCard

draw :: PlayerId -> Int -> Board -> Board
draw playerId n board =
  runGameMonad board (apply $ drawAction playerId n)

hideCard card = CardInPlay card Hidden
