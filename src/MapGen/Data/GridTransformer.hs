module MapGen.Data.GridTransformer (
  advanceGridTick,
  advanceGridTicks
) where

import qualified Data.Array as Array (Array (..), array, (!), bounds, assocs, elems)

import System.Random (RandomGen, split, random, randoms)
import Control.Monad.Random (Rand (..), Random (..), getRandom)

import MapGen.Models.Terrain (Terrain (..))
import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

advanceGridTicks :: (RandomGen a) => Int -> Grid -> Rand a Grid
advanceGridTicks 1 grid = advanceGridTick grid
advanceGridTicks i grid = advanceGridTicks (i - 1) grid >>= advanceGridTick

advanceGridTick :: (RandomGen a) => Grid -> Rand a Grid
advanceGridTick = spreadForests

spreadForests :: (RandomGen a) => Grid -> Rand a Grid

spreadForests grid = do
  let bounds = Array.bounds grid
  assocs <- sequence $ map (spreadForest grid) $ Array.assocs grid
  return $ Array.array bounds assocs 

spreadForest :: (RandomGen g) => Grid -> ((Int, Int), Tile) -> Rand g ((Int, Int), Tile)

spreadForest grid gridVal@(coords, tile) = do
  let adjacentTiles = getAdjacentTiles grid gridVal
  spread <- shouldForestSpread tile adjacentTiles
  let nextTileVal = 
        if spread
        then Tile{ terrain=Forest }
        else tile
  return (coords, nextTileVal)
   
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

shouldForestSpread :: (RandomGen g) => Tile -> [Tile] -> Rand g Bool

shouldForestSpread currentTile adjacentTiles = do
  p <- getRandom
  return (terrain currentTile == Plains && any (\tile -> terrain tile == Forest) adjacentTiles && p < (0.2 :: Float))
