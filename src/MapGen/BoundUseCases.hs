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
import MapGen.Data.MapBuilder (createMapWithFeatures)
import MapGen.Data.MapTransformer (advanceMapTicks)
import MapGen.Data.Config (Config)

boundCreateConsoleViewMap :: (RandomGen g) => g -> Int -> Int -> Config -> String

boundCreateConsoleViewMap gen width height config =
  let boundCreateMap = evalRand (runReaderT (createMapWithFeatures width height >>= advanceMapTicks 30) config) gen
      boundConsoleView = renderMap $ renderGrid renderTileTerrain
  in createConsoleViewMap boundCreateMap boundConsoleView
