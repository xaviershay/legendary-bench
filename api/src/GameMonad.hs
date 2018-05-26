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
  let (board', log) = runGameMonad' board m in

  over actionLog (\xs -> xs <> log) board'

runGameMonad' :: Board -> GameMonad Board -> (Board, S.Seq Action)
runGameMonad' board m =
  let state    = GameMonadState { _board = board } in
  let (result, log) = runIdentity $ runWriterT (runReaderT (runExceptT m) state) in

  let board' = case result of
                 Left (x, a) -> set currentAction a x
                 Right x -> x
    in

  (board', log)

currentBoard :: GameMonad Board
currentBoard = asks _board

withBoard :: Board -> GameMonad Board -> GameMonad Board
withBoard board m = do
  let state = GameMonadState { _board = board }

  local (const state) m
