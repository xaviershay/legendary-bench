{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVar, readTVar, registerDelay)
import           Control.Lens                         (over, view, set)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.STM                    (atomically, check, orElse)
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

-- time
import Data.Time.Clock (UTCTime, getCurrentTime)

import FakeData
import Types
import GameMonad
import Evaluator
import Json()

import Legendary.Extras.RFC1123 (RFC1123Time(..))
import Legendary.Extras.STM (readTVarWhen, Timeout(TimeoutSecs))

type LastModifiedHeader = Header "Last-Modified" RFC1123Time

newtype State = State
  { games :: TVar (M.HashMap Int (TVar Game)) -- TODO: Use something like ttrie to avoid giant global TVar here.
  }

mkState :: S.Seq Card -> IO State
mkState cards = do
  rng  <- liftIO newStdGen
  now  <- liftIO getCurrentTime
  gvar <- atomically . newTVar $ mkGame rng cards now
  x    <- atomically . newTVar $ M.singleton 1 gvar
  return $ State { games = x }

instance FromHttpApiData PlayerId where
  parseUrlPiece x = PlayerId <$> parseUrlPiece x

type AppM = ReaderT State Handler
type MyAPI =
       "games" :> Capture "id" Int :> QueryParam "version" Integer :> Get '[JSON] (Headers '[LastModifiedHeader] Game)
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

getGame :: Int -> Maybe Integer -> AppM (Headers '[LastModifiedHeader] Game)
getGame gameId maybeVersion = do
  let currentVersion = fromMaybe 0 maybeVersion

  gvar <- findGame gameId
  g <- liftIO $ readTVarWhen
                  gvar
                  (\x -> view (gameState . version) x > currentVersion)
                  (TimeoutSecs 30)
                >>= maybe (atomically . readTVar $ gvar) return

  -- TODO: Identify the calling player and appropriately redact.
  return
    . addHeader (RFC1123Time . view gameModified $ g)
    . over gameState (redact (PlayerId 0))
    $ g

getCards :: Int -> AppM (M.HashMap T.Text Card)
getCards gameId = do
  gvar <- findGame gameId
  g    <- liftIO . atomically . readTVar $ gvar

  return . M.fromList . fmap
    (\c -> (view templateId c, c)) $
    (cardDictionary . view gameState $ g)

handleChoice :: Int -> PlayerId -> PlayerChoice -> AppM ()
handleChoice gameId playerId choice = do
  gvar <- findGame gameId

  liftIO $ do
    now  <- getCurrentTime

    atomically . modifyTVar gvar $
      over gameState (applyChoice playerId choice) .
      set gameModified now

  return ()

  where
    applyChoice playerId choice board =
      let board' = addChoice playerId choice board in
      let incrementVersion = over version (+ 1) in

      incrementVersion . runGameMonad (mkGameMonadState board' Nothing) $
        apply (view currentAction board')

findGame :: Int -> AppM (TVar Game)
findGame gameId = do
  State{games = stateVar} <- ask

  gameMap <- liftIO . atomically . readTVar $ stateVar

  case M.lookup gameId gameMap of
    Nothing -> throwError err404
    Just gvar -> return gvar

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
