module Utils
  ( module Utils
  , module Data.Monoid
  , module Data.Foldable
  , module Debug.Trace
  , module Text.Show.Pretty
  , module Control.Lens
  ) where

import           Control.Lens     (at, ix, over, preview, set, view)
import           Data.Foldable    (toList)
import           Data.Monoid      (mempty, (<>))
import qualified Data.Sequence    as S
import qualified Data.Text        as T
import           Debug.Trace      (trace, traceM)
import           Text.Show.Pretty (ppShow)

showT :: Show a => a -> T.Text
showT = T.pack . show

moveHeadToTail :: S.Seq a -> S.Seq a
moveHeadToTail S.Empty = mempty
moveHeadToTail (x S.:<| xs) = xs S.|> x

traceMT :: Applicative t => T.Text -> t ()
traceMT = traceM . T.unpack
