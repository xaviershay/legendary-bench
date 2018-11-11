module Legendary.Extras.STM
  ( readTVarWhen
  , Timeout(..)
  ) where

import           Control.Concurrent.STM.TVar          (TVar, readTVar, registerDelay)
import           Control.Monad.STM                    (atomically, check, orElse)

data Timeout =
    TimeoutSecs Int
  | TimeoutDays Int

toMs :: Timeout -> Int
toMs (TimeoutSecs x) = x * 1000000
toMs (TimeoutDays x) = toMs (TimeoutSecs 1) * 60 * 60 * 24 * x

-- Like readTVar, but retries until the returned value passes the provided
-- check. If timeout elapses, return Nothing.
readTVarWhen :: TVar a -> (a -> Bool) -> Timeout -> IO (Maybe a)
readTVarWhen var f timeout = do
  timer <- registerDelay $ toMs timeout

  let
    timeoutAction = do
      x <- readTVar timer
      check x
      return Nothing
    readAction = do
      x <- readTVar var
      check $ f x
      return $ Just x

  atomically $ timeoutAction `orElse` readAction
