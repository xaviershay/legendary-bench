{-# LANGUAGE OverloadedStrings #-}

module Integration.Cards where

import Unit.Utils

import qualified Data.Sequence as S
import qualified Data.Set      as Set
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Data.Maybe (fromJust)

import System.Random (mkStdGen)

import Types
import CardLang
import FakeData (genBoard)

-- TODO: DRY up with app/Main.hs
readCards :: T.Text -> IO (S.Seq Card)
readCards contents =
  case parse contents of
    Left error -> (putStrLn $ "Parse error: " <> error) >> return mempty
    Right ast -> case typecheck ast of
      Left error -> (putStrLn . show $ error) >> return mempty
      Right _ -> return $ evalCards ast

test_CardsIntegration = do
  let path = "/home/xavier/Code/legendary-bench/cards/base/heroes.lisp"

  contents <- T.readFile path
  cards <- readCards contents

  let cases = toList $ fmap (forCard $ fakeBoard cards) cards

  return $ testGroup "Card smoke tests" cases

  where
    fakeBoard :: S.Seq Card -> Board
    fakeBoard cards = genBoard (mkStdGen 0) 2 cards

    forCard board card = let code = fromJust $ preview playCode card in

                   testCase (T.unpack $ view templateId card) $
                     case evalWithBoard board code of
                       (UAction _) ->  True @=? True
                       y -> error . T.unpack $ "Unexpected state: board function doesn't evaluate to an action. Got: " <> showT y
