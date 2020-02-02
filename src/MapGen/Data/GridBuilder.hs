module MapGen.Data.GridBuilder (
  createGrid,
  createGridWithForest
) where

import Debug.Trace (trace)

import qualified Data.Array.CArray as CArray (CArray (..), array, indices, elems, bounds, ixmapWithInd, ixmap, listArray)
import qualified Data.Array as Array (Array (..), array, bounds, assocs, elems, listArray)

import Control.Arrow (first)

import Math.FFT (dct2N, dct3N)
import System.Random (RandomGen, split, random)
import Data.Random.Normal (normal, normals)
import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, Rand (..), Random (..), liftRand, getRandomR, getRandom)

import MapGen.Models.Terrain (Terrain (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

import qualified MapGen.Data.Config as Config (FeatureConfig (..), getFeatureConfigs)

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

type NoiseGrid = CArray.CArray (Int, Int) Float
type HeightGrid = CArray.CArray (Int, Int) Float
type TemperatureGrid = Array.Array (Int, Int) Float

createTemperatureGrid :: RandomGen g => Int -> Int -> Rand g TemperatureGrid

createTemperatureGrid width height = do
  tempOffset <- getRandom
  tempScale <- getRandom
  let tempOffsetStrength = 2.0
      tempScaleStrength = 2.0
      bounds = ((0, 0), (width - 1, height - 1))
      tempValues = [fromIntegral y * (tempScaleStrength * tempScale) + (tempOffset * tempOffsetStrength) | x <- [0..width - 1], y <- [0..height - 1]]
  return $ Array.listArray bounds tempValues

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

transformHeightToTerrain :: Float -> Terrain
transformHeightToTerrain height
  | height < -50 = Ocean
  | height < 0 = Shallows
  | height < 100 = Plains
  | height < 200 = Hills
  | height < 300 = Mountains
  | otherwise = Peaks

createTile :: Float -> Float -> Tile
createTile height temperature = Tile{
  height=height
  ,terrain=transformHeightToTerrain height
  ,temperature=temperature
}
 
createGrid :: RandomGen g => Int -> Int -> Rand g Grid
createGrid width height = do
  temperatureVals <- Array.elems <$> createTemperatureGrid width height
  heightVals <- CArray.elems <$> createHeightGrid width height
  let bounds = ((0, 0), (width - 1, height - 1))
      gridVals = zipWith createTile heightVals temperatureVals
  return $ Array.listArray bounds gridVals

seedTileWithFeature :: (RandomGen g) => Config.FeatureConfig -> Tile -> Rand g Tile

seedTileWithFeature featureConfig tile = do
  p <- getRandomR (0.0 :: Float, 1.0 :: Float)
  let temp = temperature tile
      h = height tile
      (tempMin, tempMax) = Config.temperature featureConfig
      (heightMin, heightMax) = Config.height featureConfig
      (pSeed, _) = Config.pGrowth featureConfig
      destinationTerrain = Config.terrain featureConfig
      nextTile = if p < pSeed && temp > tempMin && temp < tempMax && h > heightMin && h < heightMax
                 then Tile{height=h, temperature=temp, terrain=destinationTerrain}
                 else tile
  return nextTile

seedGridWithFeature :: (RandomGen g) => Config.FeatureConfig -> Grid -> Rand g Grid
seedGridWithFeature featureConfig grid = do
  elems <- mapM (seedTileWithFeature featureConfig) (Array.elems grid)
  let bounds = Array.bounds grid
  return $ Array.listArray bounds elems

seedGridWithFeatures :: (RandomGen g) => [Config.FeatureConfig] -> Grid -> Rand g Grid
seedGridWithFeatures [] grid = return grid
seedGridWithFeatures (feature:otherFeatures) grid = seedGridWithFeature feature grid >>= seedGridWithFeatures otherFeatures

createGridWithForest :: (RandomGen g) => Int -> Int -> Rand g Grid
createGridWithForest width height = createGrid width height >>= seedGridWithFeatures Config.getFeatureConfigs
