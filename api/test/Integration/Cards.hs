{-# LANGUAGE OverloadedStrings #-}

module Integration.Cards where

import Unit.Utils

import qualified Data.Sequence as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Data.Maybe (fromJust)

import System.Random (mkStdGen)

import Types
import CardLang
import FakeData (genBoard)

-- TODO: DRY up with app/Main.hs
readCards :: T.Text -> IO (Either String (S.Seq Card))
readCards contents =
  case parse contents of
    Left error -> return $ Left ("Parse error: " <> error)
    Right ast -> case typecheck (mkEnv Nothing) ast of
      Left error -> return $ Left (show error)
      Right _ -> return . Right $ evalCards ast

test_CardsIntegration = do
  let prelude = "/home/xavier/Code/legendary-bench/cards/prelude.lisp"
  let path = "/home/xavier/Code/legendary-bench/cards/base/heroes.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  cards <- readCards (prelude <> "\n" <> contents)

  case cards of
    Left x -> return $ testCase "Hero smoke tests" (assertFailure x)
    Right cards -> do
      let cases = toList $ fmap (forCard $ fakeBoard cards) cards

      return $ testGroup "Hero smoke tests" cases

  where
    fakeBoard :: S.Seq Card -> Board
    fakeBoard cards = genBoard (mkStdGen 0) 2 cards

    forCard board card = let code = fromJust $ preview playCode card in
                   testCase (T.unpack $ view templateId card) $
                     let env = mkEnv (Just board) in
                     -- TODO: Evaluate all effects, not just the first one
                     case evalWith env (head . toList $ code) of
                       (UAction _) ->  True @=? True
                       y -> error . T.unpack $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> showT y

test_HenchmenIntegration = do
  let prelude = "/home/xavier/Code/legendary-bench/cards/prelude.lisp"
  let path = "/home/xavier/Code/legendary-bench/cards/base/henchmen.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  cards <- readCards (prelude <> "\n" <> contents)

  case cards of
    Left x -> return $ testCase "Henchmen smoke tests" (assertFailure x)
    Right cards -> do
      let cases = toList $ fmap (forCard $ fakeBoard cards) cards

      return $ testGroup "Henchmen smoke tests" cases

  where
    fakeBoard :: S.Seq Card -> Board
    fakeBoard cards = genBoard (mkStdGen 0) 2 cards

    forCard board card = let (_, code) = fromJust . fromJust $ preview fightCode card in
                   testCase (T.unpack $ view templateId card) $
                     let env = mkEnv (Just board) in
                     -- TODO: Evaluate all effects, not just the first one
                     case evalWith env code of
                       (UAction _) ->  True @=? True
                       y -> error . T.unpack $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> showT y
focus = test_HenchmenIntegration >>= defaultMain
