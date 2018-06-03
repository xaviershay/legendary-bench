{-# LANGUAGE OverloadedStrings #-}

module FakeData where

import           Control.Lens        (set, view)
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

mkGame :: StdGen -> S.Seq Card -> Game
mkGame g cards = Game
  { _gameState = prepareBoard $ genBoard g 2 cards
  }

genBoard :: StdGen -> Int -> S.Seq Card -> Board
genBoard g playerCount cards = evalState a 1
  where
    a = do
      let ps = S.fromList $ fmap PlayerId [0..playerCount -1]

      villainDeck   <- mkVillainDeck
      heroDeck      <- mkHeroDeck cards
      bystanderDeck <- mkBystanderDeck

      id
        . set (cardsAtLocation VillainDeck) villainDeck
        . set (cardsAtLocation HeroDeck) heroDeck
        . set (cardsAtLocation BystanderDeck) bystanderDeck
        . set players (fmap mkPlayer ps)
        . set rng g
        <$> foldM setPlayerDeck mkBoard ps

    setPlayerDeck board pid = do
      playerDeck <- mkPlayerDeck

      return $ set
        (cardsAtLocation (PlayerLocation pid PlayerDeck))
        playerDeck
        board

mkHeroDeck cards =
  sequence
    . fmap (mkCardInPlay Hidden)
    . mconcat
    . fmap (\c -> S.replicate (toInt $ view heroStartingNumber c) c)
    . toList
    $ cards

mkBystanderDeck =
  sequence . fmap (mkCardInPlay All) $
    S.replicate 30 BystanderCard

mkVillainDeck =
  sequence . fmap (mkCardInPlay Hidden) $
    S.replicate 30 villianCard <> S.replicate 30 BystanderCard


mkPlayerDeck =
  sequence . fmap (mkCardInPlay Hidden) $
       S.replicate 8 moneyCard
    <> S.replicate 4 attackCard

mkCardInPlay :: Visibility -> Card -> State Int CardInPlay
mkCardInPlay visibility template = do
  cardId <- getThen (+ 1)

  return $ CardInPlay
    { _cardId = CardId cardId
    , _cardVisibility = visibility
    , _cardTemplate = template
    }

prepareBoard board = runGameMonad board (apply ActionPrepareGame)

getThen :: (a -> a) -> State a a
getThen f = do
  x <- get
  put (f x)
  return x
