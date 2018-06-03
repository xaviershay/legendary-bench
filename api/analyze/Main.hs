module Main where

import qualified Data.Text.IO as T

import Text.Show.Pretty (ppShow)
import CardLang.Parser
import CardLang.TypeInference
import Utils

main = do
  let path = "/home/xavier/Code/legendary-bench/cards/base/heroes/spiderman.lisp"

  contents <- T.readFile path

  --printAst contents
  printType contents 

printAst contents =
  case parse contents of
    Left error -> putStrLn $ "Parse error: " <> error
    Right ast -> putStrLn . ppShow $ ast

printType contents =
  case parse contents of
    Left error -> putStrLn $ "Parse error: " <> error
    Right ast -> case typecheck ast of
      Left error -> putStrLn . show $ error
      Right t -> T.putStrLn . showType $ t

focus = main
