module MapGen.Data.MapBuilder (
  createMap
) where

import MapGen.Models.Map (Map (..))
import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Tile (Tile (..))
createMap :: Grid Tile -> Map

createMap grid = Map { grid=grid }
