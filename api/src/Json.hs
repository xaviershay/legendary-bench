{-# LANGUAGE OverloadedStrings #-}

module Json where

import Control.Lens (view)
import qualified Data.Sequence as S
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T

import Types
import Utils

instance ToJSONKey Location where
  toJSONKey = toJSONKeyText $ \x ->
    case x of
      Boss -> "boss"
      HQ -> "hq"
      HeroDeck -> "hero-deck"
      PlayerLocation (PlayerId id) location -> "player-"
                                            <> showT id
                                            <> "-"
                                            <> (T.toLower $ showT location)

instance ToJSON ScopedLocation
instance ToJSON Location

instance ToJSON GameState where
  toJSON = toJSON . T.toLower . showT

instance ToJSON Game where
  toJSON game = object
    [ "board" .= view gameState game
    ]

instance ToJSON Player where
  toJSON player = object
    [ "resources" .= view resources player
    ]

instance ToJSON Resources where
  toJSON resources = object
    [ "attack" .= view attack resources
    , "money"  .= view money resources
    ]

instance ToJSON PlayerId where
  toJSON (PlayerId id) = toJSON id

instance ToJSON Effect where
  toJSON = toJSON . show

instance ToJSON CardInPlay where
  -- Data should be pre-processed to remove all other visibility types before
  -- reaching here.
  toJSON (CardInPlay _ Owner) =
    error "Trying to convert Owner visibility to JSON"

  toJSON (CardInPlay card vis) = object $
    [ "type"    .= cardType card
    , "visible" .= toJSON visible
    ] <>
    [ "name" .= cardName card | visible
    ]

    where
      visible = vis == All

instance ToJSON IndexedPlayer where
  toJSON (IndexedPlayer (player, i)) = object
    [ "id"        .= i
    , "resources" .= view resources player
    ]

newtype IndexedPlayer = IndexedPlayer (Player, Int)

instance ToJSON Board where
  toJSON b = object
    [ "cards"   .= toJSON (view cards b)
    , "players" .= toJSON (map IndexedPlayer $ zip (toList $ view players b) [(0 :: Int)..])
    , "state"   .= toJSON (view boardState b)
    ]
