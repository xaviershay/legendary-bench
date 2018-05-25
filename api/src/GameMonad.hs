module GameMonad where

import Types
import Utils
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import qualified Data.Sequence as S

runGameMonad :: Board -> GameMonad Board -> Board
runGameMonad board m =
  -- Board must have at least one player
  let Just pid = preview (players . element 0 . playerId) board in
  let state    = GameMonadState { _activePlayer = pid, _board = board } in
  let (result, log) = runIdentity $ runWriterT (runReaderT (runExceptT m) state) in

  let board' = case result of
                 Left (x, a) -> set currentAction a x
                 Right x -> x
    in

  over actionLog (\xs -> xs <> log) board'

currentPlayer :: GameMonad PlayerId
currentPlayer = asks _activePlayer

currentBoard :: GameMonad Board
currentBoard = asks _board

withBoard :: Board -> GameMonad Board -> GameMonad Board
withBoard board m = do
  playerId <- currentPlayer

  let state = GameMonadState { _activePlayer = playerId, _board = board }

  local (const state) m
