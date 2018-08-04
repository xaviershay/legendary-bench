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
      mmDeck        <- mkMastermindDeck cards

      id
        . set (cardsAtLocation VillainDeck) villainDeck
        . set (cardsAtLocation HeroDeck) heroDeck
        . set (cardsAtLocation BystanderDeck) bystanderDeck
        . set (cardsAtLocation WoundDeck) woundDeck
        . set (cardsAtLocation MastermindDeck) mmDeck
        . set players (fmap mkPlayer ps)
        . set rng g
        <$> foldM setPlayerDeck mkBoard ps

    setPlayerDeck board pid = do
      playerDeck <- mkPlayerDeck cards

      return $ set
        (cardsAtLocation (PlayerLocation pid PlayerDeck))
        playerDeck
        board

mkMastermindDeck cards =
  traverse (mkCardInPlay All) $
       S.singleton (findMastermind "Dr. Doom")
    <> S.filter (isTacticFor "Dr. Doom") cards

  where
    findMastermind name = fromJust $ find ((==) name . view mmName) cards
    isTacticFor name = (==) name . view mmtName

mkHeroDeck =
  traverse (mkCardInPlay All)
    . mconcat
    . fmap (\c -> S.replicate (toInt $ view heroStartingNumber c) c)
    . toList

mkBystanderDeck =
  traverse (mkCardInPlay All) $
    S.replicate 30 BystanderCard

mkWoundDeck =
  traverse (mkCardInPlay All) $
    S.replicate 30 WoundCard

mkVillainDeck cards =
  traverse (mkCardInPlay Hidden) $
       S.replicate 5 MasterStrikeCard
    <> S.replicate 12 TwistCard -- This will come from scheme eventually
    <> ( mconcat
       . fmap (\c -> S.replicate (toInt $ view enemyStartingNumber c) c)
       . toList
       $ cards)

mkPlayerDeck cards =
  traverse (mkCardInPlay Hidden) $
       S.replicate 8 recruitCard
    <> S.replicate 4 attackCard
    -- <> S.replicate 5 (findCard "Hulk Smash")
    <> S.replicate 5 (findCard "Hey, Can I Get a Do-Over?")

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

prepareBoard board = runGameMonad (mkGameMonadState board Nothing) (apply ActionPrepareGame)

getThen :: (a -> a) -> State a a
getThen f = do
  x <- get
  put (f x)
  return x
