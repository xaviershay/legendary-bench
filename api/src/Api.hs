{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import GHC.Generics
import GHC.TypeLits
import Servant
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import Data.Monoid (mempty, (<>))
import Control.Monad.Reader (Reader)

import qualified Data.Text as T

import FakeData
import Utils
import Types
import Json

data State = State
  { game :: TVar Game -- TODO: Handle multiple games at once
  }

mkState :: IO State
mkState = do
  x <- atomically . newTVar $ mkGame
  return $ State { game = x }

type AppM = ReaderT State Handler
type MyAPI = "games" :> Capture "id" Int :> Get '[JSON] Game


api :: Proxy MyAPI
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $
  hoistServer api (nt s) server

server = getGame

getGame :: Int -> AppM Game
getGame id = do
  State{game = gvar} <- ask
  g <- liftIO $ atomically $ readTVar gvar

  return $ g { _gameState = redact (PlayerId 0) (_gameState g) }
  --return g
