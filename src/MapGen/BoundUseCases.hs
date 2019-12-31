module MapGen.BoundUseCases (
  boundCreateConsoleViewMap
) where

import System.Random (RandomGen)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.Views.MapConsoleView (renderMap)
import MapGen.Views.GridConsoleView (renderGrid)
import MapGen.Views.TileConsoleView (renderTile)
import MapGen.Data.MapBuilder (createMap)
import MapGen.Data.GridBuilder (createGrid)

boundCreateConsoleViewMap :: (RandomGen g) => g -> Int -> Int -> String

boundCreateConsoleViewMap gen width height =
  let boundCreateMap = createMap $ createGrid gen width height
      boundConsoleView = renderMap $ renderGrid renderTile
  in createConsoleViewMap boundCreateMap boundConsoleView
