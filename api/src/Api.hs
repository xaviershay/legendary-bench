{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Lens                (over)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (Reader)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Strict         as M
import           Data.Monoid                 (mempty, (<>))
import qualified Data.Text                   as T
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Middleware.Cors (cors, corsRequestHeaders,
                                              simpleCorsResourcePolicy)
import           Servant

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
app s =   cors (const . Just $ corsPolicy)
        $ serve api
        $ hoistServer api (nt s) server
  where
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

server = getGame

getGame :: Int -> AppM Game
getGame id = do
  State{game = gvar} <- ask
  g <- liftIO $ atomically $ readTVar gvar

  return $ g { _gameState = redact (PlayerId 0) (_gameState g) }
  --return g

redact :: PlayerId -> Board -> Board
redact id board = over cards (M.mapWithKey f) board
  where
    f (PlayerLocation owner _) cs =
      let desired = if owner == id then All else Hidden in

      fmap (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired (CardInPlay card Owner) = CardInPlay card desired
    transformOwned _ x = x
