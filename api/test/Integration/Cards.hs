{-# LANGUAGE OverloadedStrings #-}

module Integration.Cards where

import Unit.Utils

import           Control.Lens        (element)
import           Data.Maybe          (fromJust)
import qualified Data.Sequence       as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import System.Random (mkStdGen)

import Types
import Utils
import CardLang
import GameMonad
import Evaluator (apply)
import FakeData (genBoard)

-- TODO: DRY up with app/Main.hs
readCards :: T.Text -> IO (Either String (S.Seq Card))
readCards contents =
  case parse contents of
    Left error -> return $ Left ("Parse error: " <> error)
    Right ast -> case typecheck (mkEnv Nothing) ast of
      Left error -> return $ Left (show error)
      Right _ -> return $ case evalCards ast of
        Left x ->  Left $ T.unpack x
        Right y -> Right y

fakeBoard :: S.Seq Card -> Board
fakeBoard cards = let b = genBoard (mkStdGen 0) 2 cards in
                    -- Assume the first player starts first
                    case preview (players . element 0 . playerId) b of
                      Just pid -> set turnStack (S.singleton pid) b
                      _   -> error "genBoard should have added a player"


test_CardsIntegration = do
  prelude <- cardsPath "prelude.lisp"
  path <- cardsPath "base/heroes.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  cards <- readCards (prelude <> "\n" <> contents)

  case cards of
    Left x -> return $ testCase "Hero smoke tests" (assertFailure x)
    Right cards -> do
      let cases = toList $ fmap (forCard $ fakeBoard cards) cards

      return $ testGroup "Hero smoke tests" cases

  where
    forCard board card = let code = fromJust $ preview playCode card in
                   testCase (T.unpack $ view templateId card) $
                     -- Some hax here around current-card
                     let card = cardById HeroDeck (CardId 1) in

                     let board' = runGameMonad (mkGameMonadState board (Just card)) (apply code) in

                     case view boardState board' of
                       Lost x -> assertFailure (show x)
                       _ -> True @=? True

test_HenchmenIntegration = do
  prelude <- cardsPath "prelude.lisp"
  path <- cardsPath "base/henchmen.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  cards <- readCards (prelude <> "\n" <> contents)

  case cards of
    Left x -> return $ testCase "Henchmen smoke tests" (assertFailure x)
    Right cards -> do
      let cases = toList $ fmap (forCard $ fakeBoard cards) cards

      return $ testGroup "Henchmen smoke tests" cases

  where
    forCard board card = let code = fromJust $ preview fightCode card in
                   testCase (T.unpack $ view templateId card) $
                     let env = mkEnv (Just board) in
                     let board' = runGameMonad
                                    (mkGameMonadState board Nothing)
                                    (apply $ extractCode code) in

                     case view boardState board' of
                       Lost x -> assertFailure (show x)
                       _ -> True @=? True

test_MastermindIntegration = do
  prelude <- cardsPath "prelude.lisp"
  path <- cardsPath "base/masterminds.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  cards <- readCards (prelude <> "\n" <> contents)

  case cards of
    Left x -> return $ testCase "Mastermind smoke tests" (assertFailure x)
    Right cards -> do
      let cases = toList $ fmap (forCard $ fakeBoard cards) cards

      return $ testGroup "Mastermind smoke tests" cases

  where
    forCard board (card@MastermindTacticCard{}) =
      let code = fromJust $ preview mmtFightCode card in

      testCase (T.unpack $ view templateId card) $
        let env = mkEnv (Just board) in
        let board' = runGameMonad
                       (mkGameMonadState board Nothing)
                       (apply $ extractCode code) in

        case view boardState board' of
          Lost x -> assertFailure (show x)
          _ -> True @=? True
    forCard board (card@MastermindCard{}) =
      let code = fromJust $ preview mmStrikeCode card in

      testCase (T.unpack $ view templateId card) $
        let env = mkEnv (Just board) in
        let board' = runGameMonad
                       (mkGameMonadState board Nothing)
                       (apply $ extractCode code) in

        case view boardState board' of
          Lost x -> assertFailure (show x)
          _ -> True @=? True
