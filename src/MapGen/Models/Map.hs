module MapGen.Models.Map (
  Map (..)
) where

import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Tile (Tile (..))

newtype Map = Map {
  grid :: Grid Tile
} deriving (Show, Eq)
