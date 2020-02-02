module MapGen.Data.GridBuilder (
  createGrid,
  createGridWithForest,
  seedGridWithForest,
  seedTileWithForest
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

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

type NoiseGrid = CArray.CArray (Int, Int) Float
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

createFilteredNoiseGrid :: RandomGen g => Int -> Int -> Rand g NoiseGrid
createFilteredNoiseGrid = (((iftNoiseGrid . filterFTNoiseGrid . ftNoiseGrid) <$>) .) . createNoiseGrid

transformNoiseValToTerrain :: Float -> Terrain
transformNoiseValToTerrain val
  | val < -50 = Ocean
  | val < 0 = Shallows
  | val < 100 = Plains
  | val < 200 = Hills
  | val < 300 = Mountains
  | otherwise = Peaks

createTile :: Float -> Float -> Tile
createTile noiseVal temperatureVal = Tile{ 
  terrain = transformNoiseValToTerrain noiseVal
  ,temperature = temperatureVal
}
 
createGrid :: RandomGen g => Int -> Int -> Rand g Grid
createGrid width height = do
  temperatureVals <- Array.elems <$> createTemperatureGrid width height
  noiseVals <- CArray.elems <$> createFilteredNoiseGrid width height
  let bounds = ((0, 0), (width - 1, height - 1))
      gridVals = zipWith createTile noiseVals temperatureVals
  return $ Array.listArray bounds gridVals

seedTileWithForest :: (RandomGen g) => ((Int, Int), Tile) -> Rand g ((Int, Int), Tile)
seedTileWithForest original@((x, y), tile) = do
  p <- getRandomR (0.0 :: Float, 1.0 :: Float)
  let nextTile = if terrain tile == Plains && (p < 0.01) then ((x, y), Tile{temperature=temperature tile, terrain=Forest}) else original
  return nextTile

seedGridWithForest :: (RandomGen g) => Grid -> Rand g Grid
seedGridWithForest grid = do
  assocs <- mapM seedTileWithForest (Array.assocs grid)
  let bounds = Array.bounds grid
  return $ Array.array bounds assocs

createGridWithForest :: (RandomGen g) => Int -> Int -> Rand g Grid
createGridWithForest width height = createGrid width height >>= seedGridWithForest
