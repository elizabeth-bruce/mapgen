module MapGen.Views.GridConsoleView (
  renderGrid
) where

import Data.Array (bounds, elems)
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)

import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

renderGrid :: (Tile -> String) -> Grid Tile -> String

renderGrid renderTile grid =
  let (_, (_, height)) = bounds grid
      stringGrid = map concat $ transpose $ chunksOf (height + 1) $ map renderTile $ elems grid
      string = intercalate "\n" stringGrid ++ "\n"
  in string
