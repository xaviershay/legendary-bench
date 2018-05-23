{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVar, readTVar,
                                                       writeTVar)
import           Control.Lens                         (over, view)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (Reader)
import           Control.Monad.STM                    (atomically, check)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Strict                  as M
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (mempty, (<>))
import qualified Data.Text                            as T
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Timeout                       (timeout)

import FakeData
import Utils
import Types
import Json
import GameMonad
import Evaluator
import Action

newtype State = State
  { game :: TVar Game -- TODO: Handle multiple games at once
  }

mkState :: IO State
mkState = do
  x <- atomically . newTVar $ mkGame
  return $ State { game = x }

instance FromHttpApiData PlayerId where
  parseUrlPiece x = PlayerId <$> parseUrlPiece x

type AppM = ReaderT State Handler
type MyAPI =
       "games" :> Capture "id" Int :> QueryParam "version" Integer :> Get '[JSON] Game
  :<|> "games" :> Capture "id" Int :> "players" :> Capture "playerId" PlayerId :> "act" :> ReqBody '[JSON] PlayerAction :> Post '[JSON] ()

api :: Proxy MyAPI
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s =   logStdoutDev
        $ cors (const . Just $ corsPolicy)
        $ serve api
        $ hoistServer api (nt s) server
  where
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

server = getGame :<|> handleAction

getGame :: Int -> Maybe Integer -> AppM Game
getGame gameId maybeVersion = do
  let currentVersion = fromMaybe 0 maybeVersion
  State{game = gvar} <- ask

  mres <- liftIO $ timeout (30 * 1000000) (waitForNewState gvar currentVersion)

  g' <- case mres of
          Nothing -> liftIO . atomically . readTVar $ gvar
          Just g  -> return g

  return $ over gameState (redact (PlayerId 0)) g'

  where
    waitForNewState :: TVar Game -> Integer -> IO Game
    waitForNewState gvar v =
      atomically $ do
        x <- readTVar gvar
        check $ view (gameState . version) x > v
        return x

handleAction :: Int -> PlayerId -> PlayerAction -> AppM ()
handleAction gameId playerId action = do
  State{game = gvar} <- ask

  liftIO . atomically . modifyTVar gvar $
    over gameState (applyAction playerId action)

  return ()

  where
    applyAction playerId action board =
      runGameMonad playerId board $
        translatePlayerAction action >>= applyWithVersionBump

redact :: PlayerId -> Board -> Board
redact id = over cards (M.mapWithKey f)
  where
    f (PlayerLocation owner _) cs =
      let desired = if owner == id then All else Hidden in

      fmap (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired (CardInPlay card Owner) = CardInPlay card desired
    transformOwned _ x = x
