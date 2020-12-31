{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MapGen.Views.TileJsonView (
  renderTile
  ,TileJsonView
) where

import Data.Aeson

import MapGen.Models.Tile (Tile (..), HeightType (..), TemperatureType (..), PrecipitationType (..))
import MapGen.Models.Feature(Feature (..))

data TileJsonView = TileJsonView {
  temperature :: Float,
  height :: Float,
  precipitation :: Float,
  feature :: Maybe String
} deriving (Show)

instance ToJSON TileJsonView where
  toJSON (TileJsonView temperature height precipitation feature) =
    object ["temperature" .= temperature, "height" .= height, "precipitation" .= precipitation, "feature" .= feature]

renderTile :: Tile -> TileJsonView
renderTile Tile{temperature=temperature,height=height,precipitation=precipitation,feature=feature} =
  let viewFeature = name <$> feature
  in TileJsonView temperature height precipitation viewFeature
