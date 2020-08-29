{-# LANGUAGE DuplicateRecordFields #-} 

module MapGen.Data.GridBuilder (
  createGrid
) where

import Data.Array (Array (..), array, bounds, assocs, elems, listArray)

import System.Random (RandomGen, split, random)
import Data.Random.Normal (normal, normals)
import Control.Monad.Random (MonadRandom, RandT, Rand (..))

import MapGen.Models.Tile (Tile (..), createTile)
import MapGen.Models.Grid (Grid (..), GridCoordinate, GridEntry (..))

import MapGen.Data.Config (MapConfig)

import MapGen.Data.GridBuilder.HeightGridBuilder (createHeightGrid)
import MapGen.Data.GridBuilder.TemperatureGridBuilder (createTemperatureGrid)
import MapGen.Data.GridBuilder.PrecipitationGridBuilder (createPrecipitationGrid)

import qualified MapGen.Data.Config as Config (Config, FeatureConfig (..), MapConfig(..), toFeature)

type TileGrid = Grid Tile

createGrid :: RandomGen g => MapConfig -> Rand g TileGrid
createGrid mc@Config.MapConfig{height=height, width=width} = do
  heightGrid <- createHeightGrid width height
  temperatureVals <- elems <$> createTemperatureGrid heightGrid
  let heightVals = elems heightGrid
      precipitationVals = elems (createPrecipitationGrid heightGrid)
  let bounds = ((0, 0), (width - 1, height - 1))
      gridVals = zipWith3 createTile heightVals temperatureVals precipitationVals
  return $ listArray bounds gridVals
