module Utils (module Utils, module Data.Monoid)where

import qualified Data.Text as T
import Data.Monoid ((<>), mempty)

showT :: Show a => a -> T.Text
showT = T.pack . show
