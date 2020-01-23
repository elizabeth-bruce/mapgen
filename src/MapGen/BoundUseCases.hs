module MapGen.BoundUseCases (
  boundCreateConsoleViewMap
) where

import System.Random (RandomGen, split)
import Control.Monad.Random (Rand (..), evalRand)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.Views.MapConsoleView (renderMap)
import MapGen.Views.GridConsoleView (renderGrid)
import MapGen.Views.TileConsoleView (renderTile)
import MapGen.Data.MapBuilder (createMap)
import MapGen.Data.GridBuilder (createGrid, createGridWithForest)
import MapGen.Data.GridTransformer (advanceGridTicks)

boundCreateConsoleViewMap :: (RandomGen g) => g -> Int -> Int -> String

boundCreateConsoleViewMap gen width height =
  let boundCreateMapRand = createGridWithForest width height >>= advanceGridTicks 10
      boundCreateMap = createMap $ evalRand boundCreateMapRand gen
      boundConsoleView = renderMap $ renderGrid renderTile
  in createConsoleViewMap boundCreateMap boundConsoleView
