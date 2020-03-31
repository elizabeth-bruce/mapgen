module MapGen.Data.GridBuilder (
  createGrid,
  createGridWithFeatures,
  createHeightGrid
) where

import Debug.Trace

import qualified Data.Array.CArray as CArray (CArray (..), array, assocs, indices, elems, bounds, ixmapWithInd, ixmap, listArray)
import qualified Data.Array as Array (Array (..), array, bounds, assocs, elems, listArray)
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

import MapGen.Models.Tile (Tile (..), HeightType (..), TemperatureType (..), PrecipitationType (..))
import MapGen.Models.Grid (Grid (..))

import qualified MapGen.Data.Config as Config (Config, FeatureConfig (..), toFeature)

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

type NoiseGrid = CArray.CArray (Int, Int) Float
type HeightGrid = CArray.CArray (Int, Int) Float
type TemperatureGrid = Array.Array (Int, Int) Float
type PrecipitationGrid = Array.Array (Int, Int) Float

swap (x,y) = (y,x)
 
transposeGrid :: (Ix a, Ix b, Foreign.Storable.Storable e) => CArray.CArray (a,b) e -> CArray.CArray (b,a) e
transposeGrid a = CArray.ixmap (swap l, swap u) swap a where 
  (l,u) = CArray.bounds a

availPrecipitation :: (Float, Float) -> ((Int, Int), Float) -> (Float, Float)
availPrecipitation _ ((0, _), _) = (0, 0)
availPrecipitation (available, _) ((_, _), height) =
  let pAvail = minimum [available, maximum [45, height * 1]]
  in if height <= 0
    then (available + 50.0, 50)
    else (available - pAvail, pAvail)

createPrecipitationGrid :: HeightGrid -> PrecipitationGrid
createPrecipitationGrid heightGrid =
  let availPrecipitationEntries = scanl availPrecipitation (0,0) $ CArray.assocs $ transposeGrid heightGrid
      heightVals = CArray.elems heightGrid
   in Array.listArray (CArray.bounds heightGrid) $ map snd availPrecipitationEntries

getTemperatureValue :: Float -> Float -> ((Int, Int), Float) -> ((Int, Int), Float)
getTemperatureValue tempOffset tempScale ((x, y), height) =
  let tempOffsetStrength = 2.0
      tempOffsetBase = 10
      tempScaleStrength = 0.2
      tempScaleBase = 0.2
      tempBase = tempOffsetBase + (tempOffset * tempOffsetStrength) + (fromIntegral y * (tempScaleBase + tempScale * tempScaleStrength))
      heightTempRate = -0.1
      tempNew = tempBase + (maximum [height, 0] * heightTempRate)
  in ((x, y), tempNew)

createTemperatureGrid :: RandomGen g => HeightGrid -> Rand g TemperatureGrid
createTemperatureGrid heightGrid = do
  tempOffset <- getRandom
  tempScale <- getRandom
  -- TODO: Figure out a more elegant way to convert from CArray to Array
  let temperatureGrid = Array.array (CArray.bounds heightGrid) $ (getTemperatureValue tempOffset tempScale) <$> (CArray.assocs heightGrid)
  return temperatureGrid

createNoiseGrid :: RandomGen g => Int -> Int -> Rand g NoiseGrid

createNoiseGrid width height = do
  noiseValues <- take (width * height) <$> getNormals
  let bounds = ((0, 0), (width - 1, height - 1))
  return $ CArray.listArray bounds noiseValues

ftNoiseGrid :: NoiseGrid -> NoiseGrid
ftNoiseGrid = dct2N [0, 1]

filterFTNoiseGrid :: NoiseGrid -> NoiseGrid

filterFTNoiseGrid grid =
  let (_, (width, height)) = CArray.bounds grid
  in CArray.ixmapWithInd ((0, 0), (width, height)) id (\(i, j) val _ -> val * 0.15 / (fromIntegral i ** 3 + fromIntegral j ** 3 + 1 ) ** 0.5) grid

iftNoiseGrid :: NoiseGrid -> NoiseGrid
iftNoiseGrid = dct3N [0,1]

createHeightGrid :: RandomGen g => Int -> Int -> Rand g HeightGrid
createHeightGrid = (((iftNoiseGrid . filterFTNoiseGrid . ftNoiseGrid) <$>) .) . createNoiseGrid

getHeightType :: Float -> HeightType
getHeightType height
  | height <= -50 = Ocean
  | height <= 0 = Shallows
  | height < 100 = Plains
  | height < 200 = Hills
  | height < 300 = Mountains
  | otherwise = Peaks

getTemperatureType :: Float -> TemperatureType
getTemperatureType temp
  | temp < 0 = Polar
  | temp < 10 = Subpolar
  | temp < 18.5 = Temperate
  | temp < 25.0 = Subtropical
  | otherwise = Tropical

getPrecipitationType precip
  | precip < 10 = Minimal
  | precip < 30 = Low
  | precip < 50 = Medium
  | precip < 100 = High
  | otherwise = Extreme


createTile height temperature precipitation = Tile{
  height=height
  ,temperature=temperature
  ,precipitation=precipitation
  ,heightType=getHeightType height
  ,temperatureType=getTemperatureType temperature
  ,precipitationType=getPrecipitationType precipitation
  ,feature=Nothing
}
 
createGrid :: RandomGen g => Int -> Int -> Rand g Grid
createGrid width height = do
  heightGrid <- createHeightGrid width height
  temperatureVals <- Array.elems <$> createTemperatureGrid heightGrid
  let heightVals = CArray.elems heightGrid
      precipitationVals = Array.elems (createPrecipitationGrid heightGrid)
  let bounds = ((0, 0), (width - 1, height - 1))
      gridVals = zipWith3 createTile heightVals temperatureVals precipitationVals
  return $ Array.listArray bounds gridVals

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

seedGridWithFeature :: (RandomGen g) => Config.FeatureConfig -> Grid -> Rand g Grid
seedGridWithFeature featureConfig grid = do
  elems <- mapM (seedTileWithFeature featureConfig) (Array.elems grid)
  let bounds = Array.bounds grid
  return $ Array.listArray bounds elems

seedGridWithFeatures :: (RandomGen g) => Grid -> ReaderT Config.Config (Rand g) Grid
seedGridWithFeatures grid = do
  features <- ask
  lift $ foldl (\currentGrid -> \feature -> currentGrid >>= seedGridWithFeature feature) (return grid) features

createGridWithFeatures :: (RandomGen g) => Int -> Int -> ReaderT Config.Config (Rand g) Grid
createGridWithFeatures width height = lift (createGrid width height) >>= seedGridWithFeatures
