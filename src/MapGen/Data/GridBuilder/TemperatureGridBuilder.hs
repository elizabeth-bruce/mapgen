module MapGen.Data.GridBuilder.TemperatureGridBuilder(
  createTemperatureGrid
) where

import Control.Monad.Random (Rand (..), getRandom)
import Data.Array (array, assocs, bounds)
import System.Random (RandomGen)

import MapGen.Models.Grid (GridEntry, Grid (..))

type HeightGrid = Grid Float
type TemperatureGrid = Grid Float

getTemperatureValue :: Float -> Float -> GridEntry Float -> GridEntry Float
getTemperatureValue tempOffset tempScale ((x, y), height) =
  let tempOffsetStrength = 2.0
      tempOffsetBase = 0
      tempScaleStrength = 0.2
      tempScaleBase = 0.2
      tempBase = tempOffsetBase + (tempOffset * tempOffsetStrength) + (fromIntegral y * (tempScaleBase + tempScale * tempScaleStrength))
      heightTempRate = -0.01
      tempNew = tempBase + (maximum [height, 0] * heightTempRate)
  in ((x, y), tempNew)

createTemperatureGrid :: RandomGen g => HeightGrid -> Rand g TemperatureGrid
createTemperatureGrid heightGrid = do
  tempOffset <- getRandom
  tempScale <- getRandom
  -- TODO: Figure out a more elegant way to convert from CArray to Array
  let temperatureGrid = array (bounds heightGrid) $ (getTemperatureValue tempOffset tempScale) <$> (assocs heightGrid)
  return temperatureGrid


