{-# LANGUAGE OverloadedStrings #-}

module FakeData where

import           Control.Lens        (set, view)
import           Control.Monad       (foldM)
import           Control.Monad.State (State, evalState, get, put)
import           Data.Foldable       (find)
import           Data.Maybe          (fromJust)
import qualified Data.Sequence       as S
import           System.Random       (StdGen)

import Types
import Evaluator
import Utils
import GameMonad

mkGame :: StdGen -> S.Seq Card -> Game
mkGame g cards = Game
  { _gameState = prepareBoard $ genBoard g 2 cards
  }

genBoard :: StdGen -> Int -> S.Seq Card -> Board
genBoard g playerCount cards = evalState a 1
  where
    a = do
      let ps = S.fromList $ fmap PlayerId [0..playerCount -1]

      villainDeck   <- mkVillainDeck cards
      heroDeck      <- mkHeroDeck cards
      bystanderDeck <- mkBystanderDeck
      woundDeck     <- mkWoundDeck

      id
        . set (cardsAtLocation VillainDeck) villainDeck
        . set (cardsAtLocation HeroDeck) heroDeck
        . set (cardsAtLocation BystanderDeck) bystanderDeck
        . set (cardsAtLocation WoundDeck) woundDeck
        . set players (fmap mkPlayer ps)
        . set rng g
        <$> foldM setPlayerDeck mkBoard ps

    setPlayerDeck board pid = do
      playerDeck <- mkPlayerDeck cards

      return $ set
        (cardsAtLocation (PlayerLocation pid PlayerDeck))
        playerDeck
        board

mkHeroDeck =
  traverse (mkCardInPlay Hidden)
    . mconcat
    . fmap (\c -> S.replicate (toInt $ view heroStartingNumber c) c)
    . toList

mkBystanderDeck =
  traverse (mkCardInPlay All) $
    S.replicate 30 BystanderCard

mkWoundDeck =
  traverse (mkCardInPlay All) $
    S.replicate 30 WoundCard

mkVillainDeck =
  traverse (mkCardInPlay Hidden)
    . mconcat
    . fmap (\c -> S.replicate (toInt $ view enemyStartingNumber c) c)
    . toList

mkPlayerDeck cards =
  traverse (mkCardInPlay Hidden) $
       S.replicate 4 recruitCard
    <> S.replicate 4 attackCard
    <> S.replicate 1 (findCard "Unending Energy")
    <> S.replicate 1 (findCard "Random Acts of Unkindness")

  where
    recruitCard = findCard "S.H.E.I.L.D Agent"
    attackCard  = findCard "S.H.E.I.L.D Trooper"
    findCard name = fromJust $ find ((==) name . view heroAbilityName) cards

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
