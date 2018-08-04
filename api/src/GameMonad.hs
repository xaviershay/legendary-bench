module GameMonad where

import Types
import Utils
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import qualified Data.Sequence as S

runGameMonad :: GameMonadState -> GameMonad Board -> Board
runGameMonad state m =
  let (board', log) = runGameMonad' state m in

  over actionLog (<> log) board'

runGameMonad' :: GameMonadState -> GameMonad Board -> (Board, S.Seq Action)
runGameMonad' state m =
  let (result, log) = runIdentity $ runWriterT (runReaderT (runExceptT m) state) in

  let board' = case result of
                 Left (x, a) -> set currentAction a x
                 Right x -> x
    in

  (board', log)

mkGameMonadState board card =
  GameMonadState { _board = board, _currentCard = card }


currentBoard :: GameMonad Board
currentBoard = asks _board

currentCard :: GameMonad (Maybe SpecificCard)
currentCard = asks _currentCard

currentState :: GameMonad GameMonadState
currentState = ask

withBoard :: Board -> GameMonad a -> GameMonad a
withBoard board m =
  local (\x -> x { _board = board }) m

withCurrentCard :: SpecificCard -> GameMonad a -> GameMonad a
withCurrentCard card m = do
  local (\x -> x { _currentCard = Just card }) m
