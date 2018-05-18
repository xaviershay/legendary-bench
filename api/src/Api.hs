{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Api where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Servant
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import Types

data State = State
  { game :: TVar Game -- TODO: Handle multiple games at once
  }

mkGame :: Game
mkGame = Game
  { board = Board { players = mempty }
  }
mkState :: IO State
mkState = do
  x <- atomically . newTVar $ mkGame
  return $ State { game = x }

type AppM = ReaderT State Handler
type MyAPI = "games" :> Capture "id" Int :> Get '[JSON] Game

instance ToJSON Game
instance ToJSON Board
instance ToJSON Player
instance ToJSON Card
instance ToJSON Effect

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
  State{game = g} <- ask
  liftIO $ atomically $ readTVar g
