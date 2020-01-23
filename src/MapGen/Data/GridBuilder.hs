module MapGen.Data.GridBuilder (
  createGrid,
  createGridWithForest,
  seedGridWithForest,
  seedTileWithForest
) where

import Debug.Trace (trace)

import qualified Data.Array.CArray as CArray (CArray (..), array, indices, elems, bounds, ixmapWithInd, ixmap)
import qualified Data.Array as Array (Array (..), array, bounds, assocs, elems)

import Control.Arrow (first)

import Math.FFT (dct2N, dct3N)
import System.Random (RandomGen, split, random)
import Data.Random.Normal (normal, normals)
import Control.Monad.Random (MonadRandom, Rand (..), Random (..), liftRand, getRandomR)

import MapGen.Models.Terrain (Terrain (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

type NoiseGrid = CArray.CArray (Int, Int) Float

createNoiseGrid :: RandomGen g => Int -> Int -> Rand g NoiseGrid

createNoiseGrid width height = do
  noiseValues <- take (width * height) <$> getNormals
  let noiseCoords = [(i, j) | i <- [0..width - 1], j <- [0..height - 1]]
      bounds = ((0, 0), (width - 1, height - 1))
  return $ CArray.array bounds $ zip noiseCoords noiseValues

ftNoiseGrid :: NoiseGrid -> NoiseGrid
ftNoiseGrid = dct2N [0, 1]

filterFTNoiseGrid :: NoiseGrid -> NoiseGrid

filterFTNoiseGrid grid =
  let (_, (width, height)) = CArray.bounds grid
  in CArray.ixmapWithInd ((0, 0), (width, height)) id (\(i, j) -> \val -> \_ -> val * 0.15 / ((fromIntegral i) ** 3 + (fromIntegral j) ** 3 + 1 ) ** 0.5) grid

iftNoiseGrid :: NoiseGrid -> NoiseGrid
iftNoiseGrid = dct3N [0,1]

createFilteredNoiseGrid :: RandomGen g => Int -> Int -> Rand g NoiseGrid

createFilteredNoiseGrid width height = iftNoiseGrid <$> filterFTNoiseGrid <$> ftNoiseGrid <$> createNoiseGrid width height

transformNoiseValToTerrain :: Float -> Terrain

transformNoiseValToTerrain val
  | val < -50 = Ocean
  | val < 0 = Shallows
  | val < 100 = Plains
  | val < 200 = Hills
  | val < 300 = Mountains
  | otherwise = Peaks

transformNoiseValToTile :: Float -> Tile
transformNoiseValToTile val = Tile{terrain=transformNoiseValToTerrain val}
 
transformNoiseGridToGrid :: NoiseGrid -> Grid
transformNoiseGridToGrid noiseGrid =
  let (_, (width, height)) = CArray.bounds noiseGrid
      terrainGridCoords = CArray.indices noiseGrid
      terrainGridVals = map transformNoiseValToTile $ CArray.elems noiseGrid
  in Array.array ((0, 0), (width, height)) $ zip terrainGridCoords terrainGridVals

createGrid :: RandomGen g => Int -> Int -> Rand g Grid
createGrid width height = transformNoiseGridToGrid <$> createFilteredNoiseGrid width height

seedTileWithForest :: (RandomGen g) => ((Int, Int), Tile) -> Rand g ((Int, Int), Tile)
seedTileWithForest original@((x, y), tile) = do
  p <- getRandomR (0.0 :: Float, 1.0 :: Float)
  let nextTile = if terrain tile == Plains && (p < 0.01) then ((x, y), Tile{terrain=Forest}) else original
  return nextTile

seedGridWithForest :: (RandomGen g) => Grid -> Rand g Grid
seedGridWithForest grid = do
  assocs <- sequence $ map seedTileWithForest (Array.assocs grid)
  let bounds = Array.bounds grid
  return $ Array.array bounds assocs

createGridWithForest :: (RandomGen g) => Int -> Int -> Rand g Grid
createGridWithForest width height = createGrid width height >>= seedGridWithForest
