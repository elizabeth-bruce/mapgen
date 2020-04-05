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

type NoiseGrid = CArray ((Int, Int)) Float
type HeightGrid = Grid Float

getNormal :: (RandomGen g, Random a, Floating a) => Rand g a
getNormal = liftRand normal

getNormals :: (RandomGen g, Random a, Floating a) => Rand g [a]
getNormals = liftRand $ first normals . split

createNoiseGrid :: RandomGen g => Int -> Int -> Rand g NoiseGrid

createNoiseGrid width height = do
  noiseValues <- take (width * height) <$> getNormals
  let bounds = ((0, 0), (width - 1, height - 1))
  return $ listArray bounds noiseValues

ftNoiseGrid :: NoiseGrid -> NoiseGrid
ftNoiseGrid = dct2N [0, 1]

filterFTNoiseGrid :: NoiseGrid -> NoiseGrid
filterFTNoiseGrid grid =
  ixmapWithInd (bounds grid) id (\(i, j) val _ -> val * 0.15 / (fromIntegral i ** 3 + fromIntegral j ** 3 + 1 ) ** 0.5) grid

transformCArrayToArray :: (A.Ix a, Storable b) => CArray a b -> A.Array a b
transformCArrayToArray cArray =
  A.listArray (bounds cArray) (elems cArray)

createHeightGrid :: RandomGen g => Int -> Int -> Rand g HeightGrid
createHeightGrid = (((transformCArrayToArray . ftNoiseGrid . filterFTNoiseGrid . ftNoiseGrid) <$>) .) . createNoiseGrid

