module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import Api

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  run port (app state)

focus = main
