module MapGen.Models.Map (
  Map (..)
) where

import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.FeatureMap (FeatureMap (..))

data Map = Map {
  grid :: Grid Tile
  ,featureMap :: FeatureMap
} deriving (Show, Eq)
