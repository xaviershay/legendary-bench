module FakeData where

import qualified Data.Sequence       as S
import qualified Data.HashMap.Strict as M

import Types
import Evaluator
import Cards
import Utils
import GameMonad
import Action

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

mkPlayerDeck = S.replicate 1 spideyCard <> S.replicate 8 moneyCard <> S.replicate 4 attackCard

draw :: PlayerId -> Int -> Board -> Board
draw playerId n board =
  runGameMonad playerId board (apply $ drawAction playerId n)

play :: PlayerId -> Int -> Board -> Board
play id i board = (runGameMonad id board $ translatePlayerAction (PlayCard i) >>= apply)

purchase :: PlayerId -> Int -> Board -> Board
purchase id i board = (runGameMonad id board $ translatePlayerAction (PurchaseCard i) >>= apply)

