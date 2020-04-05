module MapGen.Views.MapConsoleView (
  renderMap
) where

import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Map (Map (..))
import MapGen.Models.Tile (Tile)

renderMap :: (Grid Tile -> String) -> Map -> String

renderMap renderGrid Map{grid=grid} = renderGrid grid 
