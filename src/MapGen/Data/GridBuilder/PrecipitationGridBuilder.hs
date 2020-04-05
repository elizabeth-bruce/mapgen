module MapGen.Data.GridBuilder.PrecipitationGridBuilder (
  createPrecipitationGrid
) where

import Data.Array (bounds, assocs, listArray)
import MapGen.Models.Grid (Grid (..), GridEntry)

type HeightGrid = Grid Float
type PrecipitationGrid = Grid Float

availPrecipitation :: (Float, Float) -> GridEntry Float -> (Float, Float)
availPrecipitation _ ((_, 0), _) = (0, 0)
availPrecipitation (available, _) ((_, _), height) =
  let pAvail = minimum [available, maximum [45, height * 1]]
  in if height <= 0
    then (available + 50.0, 50)
    else (available - pAvail, pAvail)

createPrecipitationGrid :: HeightGrid -> PrecipitationGrid
createPrecipitationGrid heightGrid =
  let availPrecipitationEntries = scanl availPrecipitation (0,0) $ assocs heightGrid
   in listArray (bounds heightGrid) $ map snd availPrecipitationEntries

