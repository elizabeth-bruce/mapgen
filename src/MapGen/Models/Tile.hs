module MapGen.Models.Tile (
  Tile (..),
  renderTile
) where

import MapGen.Models.Terrain (Terrain (..))

data Tile = Tile { terrain :: Terrain, height:: Float, temperature :: Float, precipitation :: Float } deriving (Show, Eq)

renderTile :: Tile -> String

renderTile tile = case terrain tile of
  Plains -> "\x1b[32;1m."
  Ocean -> "\x1b[34;1m~"
  Shallows -> "\x1b[36m~" 
  Forest -> "\x1b[32m♣"
  Hills -> "\x1b[32;1mᴖ" 
  Mountains -> "\x1b[31;1m▲"
  Peaks -> "\x1b[32;1mᴖ"
