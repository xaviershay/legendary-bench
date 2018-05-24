module GameMonad where

import Types
import Utils
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)

runGameMonad :: PlayerId -> Board -> GameMonad a -> a
runGameMonad id board m =
  let result = runIdentity $ runReaderT (runExceptT m) (GameMonadState { _activePlayer = id, _board = board }) in

  case result of
    Left _ -> undefined
    Right x -> x

currentPlayer :: GameMonad PlayerId
currentPlayer = asks _activePlayer

currentBoard :: GameMonad Board
currentBoard = asks _board

withBoard :: Board -> GameMonad a -> GameMonad a
withBoard board m = do
  playerId <- currentPlayer

  return $ runGameMonad playerId board m

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) =
  preview (cardsAtLocation location . ix i) <$> currentBoard
