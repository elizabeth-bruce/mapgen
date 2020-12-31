{-# LANGUAGE OverloadedStrings #-}

module MapGen.BoundUseCases (
  boundCreateConsoleViewMap,
  boundCreateJsonViewMap
) where

import qualified Data.ByteString.Lazy as B (ByteString (..))

import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.Random (Rand (..), evalRand)
import Data.Aeson (encode)
import Control.Monad.Reader (ask, runReaderT)
import System.Random (RandomGen)

import MapGen.UseCases.CreateConsoleViewMap (createConsoleViewMap)
import MapGen.UseCases.CreateJsonViewMap (createJsonViewMap)

import MapGen.Models.Map (Map (..))

import MapGen.Views.MapConsoleView as MCV (renderMap)
import MapGen.Views.GridConsoleView as GCV (renderGrid)
import MapGen.Views.TileConsoleView as TCV (renderTileTemperature, renderTileTerrain, renderTilePrecipitation)

import MapGen.Views.MapJsonView as MJV (renderMap)
import MapGen.Views.GridJsonView as GJV (renderGrid)
import MapGen.Views.TileJsonView as TJV (renderTile)

import MapGen.Data.MapBuilder (createMapWithFeatures)
import MapGen.Data.MapTransformer (advanceMapTicks)
import MapGen.Data.Config (Config (..), MapConfig (..))


createMap :: (RandomGen g) => ReaderT Config (Rand g) Map
createMap = do
    config <- ask
    let mapAge = (age . mapConfig) config
    let mapWidth = (width . mapConfig) config
    let mapHeight = (height . mapConfig) config
    initialMap <- createMapWithFeatures mapWidth mapHeight
    currentMap <- advanceMapTicks mapAge initialMap
    return currentMap

boundCreateConsoleViewMap :: (RandomGen g) => g -> Config -> String
boundCreateConsoleViewMap gen config =
  let boundCreateMap = evalRand (runReaderT (createMap) config) gen
      boundConsoleView = MCV.renderMap $ GCV.renderGrid TCV.renderTileTerrain
  in createConsoleViewMap boundCreateMap boundConsoleView

boundCreateJsonViewMap :: (RandomGen g) => g -> Config -> B.ByteString
boundCreateJsonViewMap gen config =
  let boundCreateMap = evalRand (runReaderT (createMap) config) gen
      boundJsonView = encode . (MJV.renderMap $ GJV.renderGrid TJV.renderTile)
  in createJsonViewMap boundCreateMap boundJsonView
