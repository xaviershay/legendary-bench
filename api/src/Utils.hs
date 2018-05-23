module Utils
  ( module Utils
  , module Data.Monoid
  , module Data.Foldable
  ) where

import qualified Data.Text as T
import Data.Monoid ((<>), mempty)
import Data.Foldable (toList)

showT :: Show a => a -> T.Text
showT = T.pack . show
