{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MapGen.Views.GridJsonView (
  renderGrid,
  GridJsonView
) where

import Data.Aeson

import Data.Array (assocs)
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

import MapGen.Views.TileJsonView (renderTile, TileJsonView)

data GridJsonViewEntry = GridJsonViewEntry {
    x :: Int,
    y :: Int,
    tile :: TileJsonView
}

instance ToJSON GridJsonViewEntry where
    toJSON (GridJsonViewEntry x y tile) =
        object ["x" .= x, "y" .= y, "tile" .= tile]

type GridJsonView = [GridJsonViewEntry]

renderGrid :: (Tile -> TileJsonView) -> Grid Tile -> GridJsonView
renderGrid renderTile grid =
    let mapAssocToViewEntry = \((x, y), tile) -> GridJsonViewEntry x y $ renderTile tile
    in map mapAssocToViewEntry $ assocs grid
