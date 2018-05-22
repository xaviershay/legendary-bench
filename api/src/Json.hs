{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Lens (view)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)

import Types
import Utils

instance ToJSONKey Location where
  toJSONKey = toJSONKeyText $ \x ->
    case x of
      Boss -> "boss"
      HQ -> "hq"
      HeroDeck -> "hero-deck"
      PlayerLocation id location ->   "player-"
                                    <> showT id
                                    <> "-"
                                    <> showT location

instance ToJSON Game
instance ToJSON Player
instance ToJSON Card
instance ToJSON ScopedLocation
instance ToJSON Location
instance ToJSON Visibility
instance ToJSON PlayerId
instance ToJSON GameState
instance ToJSON Resources

instance ToJSON Effect where
  toJSON = toJSON . show

instance ToJSON CardInPlay where
  toJSON (CardInPlay card Hidden) = object
    [ "type" .= ("hidden" :: String)]
  toJSON (CardInPlay card All) = toJSON card

  -- Data should be pre-processed to remove all other visibility types before
  -- reaching here.
  toJSON (CardInPlay _ Owner) =
    error "Trying to convert Owner visibility to JSON"

instance ToJSON Board where
  toJSON b = object
    [ "card"    .= toJSON (view cards b)
    , "players" .= toJSON (view players b)
    , "state"   .= toJSON (view boardState b)
    ]
