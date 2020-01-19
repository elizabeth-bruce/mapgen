module MapGen.Data.GridTransformer (
  advanceGrid
) where

import qualified Data.Array as Array (Array (..), array, (!), bounds, assocs, elems)

import System.Random (RandomGen, split, random, randoms)

import MapGen.Models.Terrain (Terrain (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

advanceGrid :: (RandomGen a) => a -> Grid -> Grid
advanceGrid = spreadForests

spreadForests :: (RandomGen a) => a -> Grid -> Grid

spreadForests gen grid =
  let bounds@(_,(width, height)) = Array.bounds grid
      mapSize = (width + 1) * (height + 1)
      probVals = take mapSize $ randoms gen :: [Float]
      assocs = map (\(prob, gridVal@((x, y), _)) -> ((x, y), spreadForest grid gridVal prob)) $ zip probVals $ Array.assocs grid
  in Array.array bounds assocs 

spreadForest :: Grid -> ((Int, Int), Tile) -> Float -> Tile

spreadForest grid gridVal@(_, tile) probability =
  let adjacentTiles = getAdjacentTiles grid gridVal
      nextTileVal = 
        if shouldForestSpread tile adjacentTiles probability
        then Tile{ terrain=Forest }
        else tile
  in nextTileVal
   
getAdjacentTiles :: Grid -> ((Int, Int), Tile) -> [Tile]

getAdjacentTiles grid ((x, y), _) =
  let ((xMin, yMin), (xMax, yMax)) = Array.bounds grid
      left = 
        if x == xMin
        then []
        else [grid Array.! (x - 1, y)]
      right =
        if x == xMax
        then []
        else [grid Array.! (x + 1, y)]
      top =
        if y == yMin
        then []
        else [grid Array.! (x, y - 1)]
      bottom =
        if y == yMax
        then []
        else [grid Array.! (x, y + 1)]
  in left ++ right ++ top ++ bottom

shouldForestSpread :: Tile -> [Tile] -> Float -> Bool

shouldForestSpread currentTile adjacentTiles probability =
  terrain currentTile == Plains
  && any (\tile -> terrain tile == Forest) adjacentTiles
  && probability < 0.20
