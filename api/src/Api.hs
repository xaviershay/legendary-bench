{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Data.Monoid (mempty, (<>))
import Data.Aeson.Types (toJSONKeyText)
import Control.Monad.Reader (Reader)

import qualified Data.Text as T

data State = State
  { game :: TVar Game -- TODO: Handle multiple games at once
  }

mkState :: IO State
mkState = do
  x <- atomically . newTVar $ mkGame
  return $ State { game = x }

type AppM = ReaderT State Handler
type MyAPI = "games" :> Capture "id" Int :> Get '[JSON] Game

instance ToJSONKey Location where
  toJSONKey = toJSONKeyText $ \x ->
    case x of
      Boss -> "boss"
      PlayerLocation id location ->   "player-"
                                    <> showT id
                                    <> "-"
                                    <> showT location

showT :: Show a => a -> T.Text
showT = T.pack . show

instance ToJSON Game
instance ToJSON Player
instance ToJSON Card
instance ToJSON Effect
instance ToJSON ScopedLocation
instance ToJSON Location
instance ToJSON Visibility
instance ToJSON PlayerId
instance ToJSON Board

instance ToJSON CardInPlay where
  toJSON (CardInPlay card Hidden) = object
    [ "type" .= ("hidden" :: String)]
  toJSON (CardInPlay card All) = toJSON card

  -- Data should be pre-processed to remove all other visibility types before
  -- reaching here.
  toJSON (CardInPlay _ _) = undefined

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
