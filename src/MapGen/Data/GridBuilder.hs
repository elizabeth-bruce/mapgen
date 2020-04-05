module MapGen.Data.GridBuilder (
  createGrid,
  createGridWithFeatures,
  createHeightGrid
) where

import Debug.Trace

import Data.Array (Array (..), array, bounds, assocs, elems, listArray)
import Data.Ix (Ix)
import Foreign.Storable (Storable)

import Control.Arrow (first)

import Math.FFT (dct2N, dct3N)
import System.Random (RandomGen, split, random)
import Data.Random.Normal (normal, normals)
import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, RandT, Rand (..), Random (..), liftRand, getRandomR, getRandom)
import Control.Monad.Reader (ReaderT, ask, local)
import Control.Monad.Trans.Class (lift)

import MapGen.Models.Tile (Tile (..), createTile)
import MapGen.Models.Grid (Grid (..), GridCoordinate, GridEntry (..))

import MapGen.Data.GridBuilder.HeightGridBuilder (createHeightGrid)
import MapGen.Data.GridBuilder.TemperatureGridBuilder (createTemperatureGrid)
import MapGen.Data.GridBuilder.PrecipitationGridBuilder (createPrecipitationGrid)

import qualified MapGen.Data.Config as Config (Config, FeatureConfig (..), toFeature)

type TileGrid = Grid Tile

createGrid :: RandomGen g => Int -> Int -> Rand g TileGrid
createGrid width height = do
  heightGrid <- createHeightGrid width height
  temperatureVals <- elems <$> createTemperatureGrid heightGrid
  let heightVals = elems heightGrid
      precipitationVals = elems (createPrecipitationGrid heightGrid)
  let bounds = ((0, 0), (width - 1, height - 1))
      gridVals = zipWith3 createTile heightVals temperatureVals precipitationVals
  return $ listArray bounds gridVals

seedTileWithFeature :: (RandomGen g) => Config.FeatureConfig -> Tile -> Rand g Tile

seedTileWithFeature featureConfig tile = do
  p <- getRandom
  let temp = temperature tile
      h = height tile
      precip = precipitation tile
      (tempMin, tempMax) = Config.temperature featureConfig
      (heightMin, heightMax) = Config.height featureConfig
      (pSeed, _) = Config.growth featureConfig
      feature = Config.toFeature featureConfig
      nextTile = if p < pSeed && temp > tempMin && temp < tempMax && h > heightMin && h < heightMax
                 then tile {feature=Just feature}
                 else tile
  return nextTile

seedGridWithFeature :: (RandomGen g) => Config.FeatureConfig -> TileGrid -> Rand g TileGrid
seedGridWithFeature featureConfig grid = do
  elems <- mapM (seedTileWithFeature featureConfig) (elems grid)
  return $ listArray (bounds grid) elems

seedGridWithFeatures :: (RandomGen g) => TileGrid -> ReaderT Config.Config (Rand g) TileGrid
seedGridWithFeatures grid = do
  features <- ask
  lift $ foldl (\currentGrid -> \feature -> currentGrid >>= seedGridWithFeature feature) (return grid) features

createGridWithFeatures :: (RandomGen g) => Int -> Int -> ReaderT Config.Config (Rand g) TileGrid
createGridWithFeatures width height = lift (createGrid width height) >>= seedGridWithFeatures
