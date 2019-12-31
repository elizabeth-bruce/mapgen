module MapGen.Data.GridBuilder (
  createGrid
) where

import qualified Data.Array.CArray as CArray (CArray (..), array, indices, elems, bounds, ixmapWithInd, ixmap)
import qualified Data.Array as Array (Array (..), array)

import Math.FFT (dct2N, dct3N)
import System.Random (RandomGen)
import Data.Random.Normal (normals)

import MapGen.Models.Terrain (Terrain (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

type NoiseGrid = CArray.CArray (Int, Int) Float

createNoiseGrid :: RandomGen g => g -> Int -> Int -> NoiseGrid

createNoiseGrid gen width height =
  let noiseValues = take (width * height) $ normals gen
      noiseCoords = [(i, j) | i <- [0..width - 1], j <- [0..height - 1]]
  in CArray.array ((0, 0), (width - 1, height - 1)) $ zip noiseCoords noiseValues

ftNoiseGrid :: NoiseGrid -> NoiseGrid
ftNoiseGrid = dct2N [0, 1]

filterFTNoiseGrid :: NoiseGrid -> NoiseGrid

filterFTNoiseGrid grid =
  let (_, (width, height)) = CArray.bounds grid
  in CArray.ixmapWithInd ((0, 0), (width, height)) id (\(i, j) -> \val -> \_ -> val * 0.15 / ((fromIntegral i) ** 3 + (fromIntegral j) ** 3 + 1 ) ** 0.5) grid

iftNoiseGrid :: NoiseGrid -> NoiseGrid
iftNoiseGrid = dct3N [0,1]

createFilteredNoiseGrid :: RandomGen g => g -> Int -> Int -> NoiseGrid

createFilteredNoiseGrid gen width height =
  let baseNoiseGrid = createNoiseGrid gen width height
  in  iftNoiseGrid . filterFTNoiseGrid . ftNoiseGrid $ baseNoiseGrid

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

createGrid :: (RandomGen g) => g -> Int -> Int -> Grid

createGrid gen width height = transformNoiseGridToGrid $ createFilteredNoiseGrid gen width height
