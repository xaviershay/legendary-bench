{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Legendary.Extras.RFC1123
  ( RFC1123Time(..)
  ) where

-- bytestring
import qualified Data.ByteString.Char8

-- servant-server
import           Servant (ToHttpApiData, toHeader, toUrlPiece)

-- time
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (FormatTime, defaultTimeLocale, formatTime)

newtype RFC1123Time = RFC1123Time UTCTime
  deriving (Show, Data.Time.Format.FormatTime)

instance ToHttpApiData RFC1123Time where
  toHeader =
    let rfc1123DateFormat = "%a, %_d %b %Y %H:%M:%S GMT" in
    Data.ByteString.Char8.pack . formatTime defaultTimeLocale rfc1123DateFormat
  toUrlPiece = error "Not intented to be used in URLs"

