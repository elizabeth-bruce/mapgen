module MapGen.Views.MapConsoleView (
  renderMap
) where

import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Map (Map (..))

renderMap :: (Grid -> String) -> Map -> String

renderMap renderGrid Map{grid=grid} = renderGrid grid 
