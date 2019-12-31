module MapGen.Data.MapBuilder (
  createMap
) where

import MapGen.Models.Map (Map (..))
import MapGen.Models.Grid (Grid (..))

createMap :: Grid -> Map

createMap grid = Map { grid=grid }
