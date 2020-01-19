module MapGen.BoundUseCases (
  boundCreateConsoleViewMap
) where

import System.Random (RandomGen, split)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.Views.MapConsoleView (renderMap)
import MapGen.Views.GridConsoleView (renderGrid)
import MapGen.Views.TileConsoleView (renderTile)
import MapGen.Data.MapBuilder (createMap)
import MapGen.Data.GridBuilder (createGridWithForest)
import MapGen.Data.GridTransformer (advanceGrid)

boundCreateConsoleViewMap :: (RandomGen g) => g -> Int -> Int -> String

boundCreateConsoleViewMap gen width height =
  let (a, b) = split gen
      (c, d) = split a
      (e, f) = split c
      boundCreateMap = createMap $ advanceGrid f $ advanceGrid e $ advanceGrid a $ advanceGrid b $ advanceGrid c $ advanceGrid d $ createGridWithForest gen width height
      boundConsoleView = renderMap $ renderGrid renderTile
  in createConsoleViewMap boundCreateMap boundConsoleView
