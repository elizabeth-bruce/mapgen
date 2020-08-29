module MapGen.BoundUseCases (
  boundCreateConsoleViewMap
) where

import System.Random (RandomGen)
import Control.Monad.Random (Rand (..), evalRand)
import Control.Monad.Reader (ask, runReaderT)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.Views.MapConsoleView (renderMap)
import MapGen.Views.GridConsoleView (renderGrid)
import MapGen.Views.TileConsoleView (renderTileTemperature, renderTileTerrain, renderTilePrecipitation)
import MapGen.Data.MapBuilder (createMapWithFeatures)
import MapGen.Data.MapTransformer (advanceMapTicks)
import MapGen.Data.Config (Config (..), MapConfig (..))

boundCreateConsoleViewMap :: (RandomGen g) => g -> Config -> String

boundCreateConsoleViewMap gen config =
  let createMapFn = do
        config <- ask
        let mapAge = (age . mapConfig) config
        let mapWidth = (width . mapConfig) config
        let mapHeight = (height . mapConfig) config
        initialMap <- createMapWithFeatures mapWidth mapHeight
        currentMap <- advanceMapTicks mapAge initialMap
        return currentMap
      boundCreateMap = evalRand (runReaderT (createMapFn) config) gen
      boundConsoleView = renderMap $ renderGrid renderTileTerrain
  in createConsoleViewMap boundCreateMap boundConsoleView
