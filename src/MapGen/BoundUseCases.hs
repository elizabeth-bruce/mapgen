module MapGen.BoundUseCases (
  boundCreateConsoleViewMap
) where

import System.Random (RandomGen)
import Control.Monad.Random (Rand (..), evalRand)
import Control.Monad.Reader (runReaderT)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.Views.MapConsoleView (renderMap)
import MapGen.Views.GridConsoleView (renderGrid)
import MapGen.Views.TileConsoleView (renderTileTemperature, renderTileTerrain, renderTilePrecipitation)
import MapGen.Data.MapBuilder (createMap)
import MapGen.Data.GridBuilder (createGridWithFeatures)
import MapGen.Data.GridTransformer (advanceGridTicks)
import MapGen.Data.Config (Config)

boundCreateConsoleViewMap :: (RandomGen g) => g -> Int -> Int -> Config -> String

boundCreateConsoleViewMap gen width height config =
  let boundCreateMapRand = createGridWithFeatures width height >>= advanceGridTicks 30
      boundCreateMap = createMap $ evalRand (runReaderT boundCreateMapRand config) gen
      boundConsoleView = renderMap $ renderGrid renderTileTerrain
  in createConsoleViewMap boundCreateMap boundConsoleView
