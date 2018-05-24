module GameMonad where

import Types
import Utils
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)

runGameMonad :: PlayerId -> Board -> GameMonad Board -> Board
runGameMonad id board m =
  let result = runIdentity $ runReaderT (runExceptT m) (GameMonadState { _activePlayer = id, _board = board }) in

  case result of
    Left (x, a) -> set currentAction a x
    Right x -> x

currentPlayer :: GameMonad PlayerId
currentPlayer = asks _activePlayer

currentBoard :: GameMonad Board
currentBoard = asks _board

withBoard :: Board -> GameMonad Board -> GameMonad Board
withBoard board m = do
  playerId <- currentPlayer

  let state = GameMonadState { _activePlayer = playerId, _board = board }

  local (const state) m

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) =
  preview (cardsAtLocation location . ix i) <$> currentBoard
