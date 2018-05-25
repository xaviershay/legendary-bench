{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Json where

import Control.Lens (view)
import qualified Data.Sequence as S
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText, Parser)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Types
import Utils

instance ToJSONKey Location where
  toJSONKey = toJSONKeyText showLocation

instance ToJSON ScopedLocation where
  toJSON = toJSON . T.toLower . showT

showLocation :: Location -> T.Text
showLocation = \case
      Boss -> "boss"
      HQ -> "hq"
      KO -> "ko"
      HeroDeck -> "hero-deck"
      VillianDeck -> "villian-deck"
      City i -> "city-" <> showT i
      Escaped -> "escaped"
      PlayerLocation (PlayerId id) location -> "player-"
                                            <> showT id
                                            <> "-"
                                            <> T.toLower (showT location)

readScopedLocation :: T.Text -> Maybe ScopedLocation
readScopedLocation x = lookup x candidates
  where
    ls = [(minBound :: ScopedLocation)..]
    candidates = zip
                   (fmap (T.toLower . showT) ls)
                   ls

instance FromJSON ScopedLocation where
  parseJSON = withText "ScopedLocation" $ \v ->
    case readScopedLocation v of
      Just x -> return x
      Nothing -> fail "No parse"


instance ToJSON Location where
  toJSON = toJSON . showLocation

instance FromJSON Location where
  parseJSON = withText "Location" $ \v ->
    let tokens = T.splitOn "-" v in

    case tokens of
      ["hq"] -> return HQ
      ["city", i] -> City <$> readError i

      ["player", i, location] -> do
        i' <- readError i
        location' <- parseJSON (Data.Aeson.String location)

        return $ PlayerLocation (PlayerId i') location'
      _ -> fail $ "Unknown location" <> T.unpack v

readError :: Read a => T.Text -> Parser a
readError x = case readMaybe . T.unpack $ x of
                Just x' -> return x'
                Nothing -> fail "Could not parse"

instance FromJSON PlayerChoice where
  parseJSON = withObject "PlayerChoice" $ \v -> do
    action <- v .: "type"

    case action of
      "ChooseCard"    -> ChooseCard <$> v .: "card"
      "ChooseEndTurn" -> return ChooseEndTurn
      _ -> fail $ "Unknown choice: " <> action

instance ToJSON GameState where
  toJSON (WaitingForChoice desc) = object
    [ "tag" .= ("waiting" :: String)
    , "description" .= desc
    ]

  toJSON Preparing = object ["tag" .= ("preparing" :: String)]
  toJSON Won = object ["tag" .= ("won" :: String)]
  toJSON (Lost reason) = object ["tag" .= ("lost" :: String), "status" .= reason]

instance ToJSON Card where
  toJSON c@HeroCard{} = object
    [ "type" .= cardType c
    , "name" .= cardName c
    , "cost" .= cardCost c
    , "baseMoney"  .= baseResource extractMoney c
    , "baseAttack" .= baseResource extractAttack c
    ]
  toJSON c@EnemyCard{} = object
    [ "type" .= cardType c
    , "name" .= cardName c
    , "health" .= cardHealth c
    ]

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
    , "version" .= toJSON (view version b)
    , "log"     .= toJSON (view actionLog b)
    ]

instance ToJSON Visibility

instance ToJSON MoveDestination where
  toJSON Front = "front"
  toJSON (LocationIndex i) = toJSON i

instance ToJSON Action where
  toJSON (ActionCombine a ActionNone) = toJSON a
  toJSON (ActionCombine ActionNone b) = toJSON b
  toJSON a@(ActionCombine _ _) = object
    [ "type" .= ("sequence" :: String)
    , "actions" .= f a
    ]

    where
      f (ActionCombine a b) = f a <> f b
      f ActionNone = []
      f x = [x]
  toJSON (ActionTagged tag action) = object
    [ "type" .= ("tagged" :: String)
    , "tag"  .= tag
    , "action" .= action
    ]
  toJSON (RevealCard specificCard vis) = object
    [ "type" .= ("reveal" :: String)
    , "target" .= specificCard
    , "visibility" .= vis
    ]
  toJSON (MoveCard specificCard location dest) = object
    [ "type" .= ("move" :: String)
    , "target" .= specificCard
    , "to"   .= location
    , "order" .= dest
    ]
  toJSON ActionNone = object ["type" .= ("none" :: String)]
  toJSON a = error $ "No JSON pattern for " <> show a
