{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module MapGen.Views.MapJsonView (
  renderMap,
  MapJsonView
) where

import Data.Aeson

import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))
import MapGen.Models.Map as M (Map (..))

import MapGen.Views.GridJsonView (renderGrid, GridJsonView)

data MapJsonView = MapJsonView {
    grid :: GridJsonView
}

instance ToJSON MapJsonView where 
    toJSON (MapJsonView grid) =
        object ["grid" .= grid]

renderMap :: (Grid Tile -> GridJsonView) -> M.Map -> MapJsonView
renderMap renderGrid = MapJsonView . renderGrid . M.grid
