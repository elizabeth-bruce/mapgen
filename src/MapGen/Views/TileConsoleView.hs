module MapGen.Views.TileConsoleView (
  renderTile
) where

import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Terrain (Terrain (..))

renderTile tile = case terrain tile of
  Plains -> "\x1b[32;1m."
  Ocean -> "\x1b[34;1m~"
  Shallows -> "\x1b[36m~"
  Forest -> "\x1b[32m♣"
  Hills -> "\x1b[32;1mᴖ"
  Mountains -> "\x1b[31;1m▲"
  Peaks -> "\x1b[30;1m▲"
