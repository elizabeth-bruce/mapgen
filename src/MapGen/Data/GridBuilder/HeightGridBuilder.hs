module MapGen.Data.GridBuilder.HeightGridBuilder (
  createHeightGrid
) where

import Control.Arrow (first)
import Control.Monad.Random (liftRand, Rand (..), Random (..))
import Data.Array.CArray (CArray (..), bounds, elems, ixmapWithInd, listArray)
import qualified Data.Array as A (Array (..), Ix, listArray)
import Data.Random.Normal (normal, normals)
import Foreign.Storable (Storable)
import Math.FFT (dct2N)
import System.Random (RandomGen, Random, split)

import MapGen.Models.Grid (Grid (..))

import MapGen.Data.Config (MapConfig (..), FrequencyConfig(..))

type NoiseGrid = CArray ((Int, Int)) Float
type HeightGrid = Grid Float

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

createNoiseGrid :: RandomGen g => MapConfig -> Rand g NoiseGrid

createNoiseGrid MapConfig{width=width, height=height} = do
  noiseValues <- take (width * height) <$> getNormals
  let bounds = ((0, 0), (width - 1, height - 1))
  return $ listArray bounds noiseValues

ftNoiseGrid :: NoiseGrid -> NoiseGrid
ftNoiseGrid = dct2N [0, 1]

filterFTNoiseGrid :: MapConfig -> NoiseGrid -> NoiseGrid
filterFTNoiseGrid MapConfig{roughness=roughness, frequency=FrequencyConfig{decayX=decayX, decayY=decayY, decayXY=decayXY}} grid =
  ixmapWithInd (bounds grid) id (\(i, j) val _ -> val * roughness / (fromIntegral i ** decayX + fromIntegral j ** decayY + 1 ) ** decayXY) grid

transformCArrayToArray :: (A.Ix a, Storable b) => CArray a b -> A.Array a b
transformCArrayToArray cArray =
  A.listArray (bounds cArray) (elems cArray)

createHeightGrid :: RandomGen g => MapConfig -> Rand g HeightGrid
createHeightGrid mapConfig = ((transformCArrayToArray . ftNoiseGrid . (filterFTNoiseGrid mapConfig) . ftNoiseGrid) <$>) $ createNoiseGrid mapConfig
