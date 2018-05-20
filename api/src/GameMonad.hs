module GameMonad where

import Types
import Utils
import Control.Lens
import Control.Monad.Reader

runGameMonad :: PlayerId -> Board -> GameMonad a -> a
runGameMonad id board m = runReader m
  (GameMonadState { _activePlayer = id, _board = board })

currentPlayer :: GameMonad PlayerId
currentPlayer = _activePlayer <$> ask

currentBoard :: GameMonad Board
currentBoard = _board <$> ask

withBoard :: Board -> GameMonad a -> GameMonad a
withBoard board m = do
  playerId <- currentPlayer

  let state = GameMonadState { _activePlayer = playerId, _board = board}

  return $ runReader m state

lookupCard :: SpecificCard -> GameMonad (Maybe CardInPlay)
lookupCard (location, i) =
  preview (cardsAtLocation location . ix i) <$> currentBoard
