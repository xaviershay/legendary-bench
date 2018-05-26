{-# LANGUAGE OverloadedStrings #-}

module FakeData where

import           Control.Lens        (set)
import           Control.Monad       (foldM)
import           Control.Monad.State (State, evalState, get, put)
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
  { _gameState = prepareBoard $ newMkBoard g 1
  }

newMkBoard :: StdGen -> Int -> Board
newMkBoard g playerCount = evalState a 1
  where
    a = do
      let ps = S.fromList $ fmap PlayerId [0..playerCount -1]

      villainDeck <- mkVillainDeck
      heroDeck <- mkHeroDeck

      id
        . set (cardsAtLocation VillainDeck) villainDeck
        . set (cardsAtLocation HeroDeck) heroDeck
        . set players (fmap mkPlayer ps)
        . set rng g
        <$> foldM setPlayerDeck mkBoard ps

    setPlayerDeck board pid = do
      playerDeck <- mkNewPlayerDeck

      return $ set
        (cardsAtLocation (PlayerLocation pid PlayerDeck))
        playerDeck
        board

mkHeroDeck =
  sequence . fmap (mkNewCardInPlay Hidden) $
    S.replicate 30 spideyCard

mkVillainDeck =
  sequence . fmap (mkNewCardInPlay Hidden) $
    S.replicate 30 villianCard

mkNewPlayerDeck =
  sequence . fmap (mkNewCardInPlay Hidden) $
       S.replicate 8 moneyCard
    <> S.replicate 4 attackCard


getThen :: (a -> a) -> State a a
getThen f = do
  x <- get
  put (f x)
  return x

mkNewCardInPlay :: Visibility -> Card -> State Int CardInPlay
mkNewCardInPlay visibility template = do
  cardId <- getThen (+ 1)

  return $ CardInPlay
    { _cardId = CardId cardId
    , _cardVisibility = visibility
    , _cardTemplate = template
    }

mkPlayerDeck = S.replicate 1 spideyCard <> S.replicate 8 moneyCard <> S.replicate 4 attackCard

prepareBoard board = runGameMonad board (apply ActionPrepareGame)

hideCard card = mkCardInPlay card Hidden

mkCardInPlay template vis = CardInPlay
  { _cardTemplate = template
  , _cardVisibility = vis
  , _cardId = CardId 0
  }
