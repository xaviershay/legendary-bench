{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVar, readTVar)
import           Control.Lens                         (over, view)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.STM                    (atomically, check)
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Strict                  as M
import           Data.Maybe                           (fromMaybe)
import qualified Data.Sequence                        as S
import qualified Data.Text                            as T
import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           System.Random                        (newStdGen)
import           System.Timeout                       (timeout)

import FakeData
import Types
import GameMonad
import Evaluator
import Json()

newtype State = State
  { game :: TVar Game -- TODO: Handle multiple games at once
  }

mkState :: S.Seq Card -> IO State
mkState cards = do
  rng <- liftIO newStdGen
  x <- atomically . newTVar $ mkGame rng cards
  return $ State { game = x }

instance FromHttpApiData PlayerId where
  parseUrlPiece x = PlayerId <$> parseUrlPiece x

type AppM = ReaderT State Handler
type MyAPI =
       "games" :> Capture "id" Int :> QueryParam "version" Integer :> Get '[JSON] Game
  :<|> "games" :> Capture "id" Int :> "cards" :> Get '[JSON] (M.HashMap T.Text Card)
  :<|> "games" :> Capture "id" Int :> "players" :> Capture "playerId" PlayerId :> "choose" :> ReqBody '[JSON] PlayerChoice :> Post '[JSON] ()

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

server = getGame :<|> getCards :<|> handleChoice

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

getCards :: Int -> AppM (M.HashMap T.Text Card)
getCards gameId = do
    State{game = gvar} <- ask
    g <- liftIO . atomically . readTVar $ gvar

    return . M.fromList . fmap
      (\c -> (view templateId c, c)) $
      (cardDictionary . view gameState $ g)

handleChoice :: Int -> PlayerId -> PlayerChoice -> AppM ()
handleChoice gameId playerId choice = do
  State{game = gvar} <- ask

  g <- liftIO . atomically . readTVar $ gvar
  --liftIO . putStrLn . show $ view (gameState . currentAction) g
  liftIO . atomically . modifyTVar gvar $
    over gameState (applyChoice playerId choice)

  g <- liftIO . atomically . readTVar $ gvar

  --liftIO $ sequence (fmap (putStrLn . show) $ view (gameState . actionLog) g)
  --liftIO . putStrLn . show $ view (gameState . currentAction) g

  return ()

  where
    incrementVersion = over version (+ 1)
    applyChoice playerId choice board =
      let board' = addChoice playerId choice board in

      incrementVersion . runGameMonad board' $
        apply (view currentAction board')

redact :: PlayerId -> Board -> Board
redact id = over cards (M.mapWithKey f)
  where
    f (PlayerLocation owner _) cs =
      --let desired = if owner == id then All else Hidden in
      -- TODO: Allow all players to see everything for ease of testing (can
      -- play all players)
      let desired = All in

      fmap (transformOwned desired) cs

    f _ cs = cs

    transformOwned desired =
      over cardVisibility (\x -> if x == Owner then desired else x)
