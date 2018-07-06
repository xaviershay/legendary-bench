{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Sequence   as S

import Control.Monad (forM)

import Api
import Types
import Utils
import CardLang

main :: IO ()
main = do
  let prelude = "/home/xavier/Code/legendary-bench/cards/prelude.lisp"
  let path = "/home/xavier/Code/legendary-bench/cards/base/heroes.lisp"
  let henchmen = "/home/xavier/Code/legendary-bench/cards/base/henchmen.lisp"

  prelude <- T.readFile prelude
  contents <- T.readFile path
  contents2 <- T.readFile henchmen
  cards <- readCards (prelude <> "\n" <> contents <> contents2)

  forM cards $ \x -> do
    --putStrLn ""
    putStrLn . T.unpack $ view templateId x
    --putStrLn . ppShow $ view playCode x

  let port = 8080
  state <- mkState cards
  run port (app state)

readCards :: T.Text -> IO (S.Seq Card)
readCards contents =
  let env = mkEnv Nothing in
  case parse contents of
    Left error -> (putStrLn $ "Parse error: " <> error) >> return mempty
    Right ast -> case typecheck env ast of
      Left e -> error $ show e
      Right _ -> return $ evalCards ast

focus = main
