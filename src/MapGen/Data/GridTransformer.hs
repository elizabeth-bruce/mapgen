module MapGen.Data.GridTransformer (
  advanceGridTick,
  advanceGridTicks
) where

import qualified Data.Array as Array (Array (..), array, (!), bounds, assocs, elems)

import System.Random (RandomGen, split, random, randoms)
import Control.Monad.Random (Rand (..), Random (..), getRandom)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (ReaderT, ask)

import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Grid (Grid (..))

import qualified MapGen.Data.Config as Config (Config, FeatureConfig (..), toFeature)

advanceGridTicks :: (RandomGen a) => Int -> Grid -> ReaderT Config.Config (Rand a) Grid
advanceGridTicks 1 grid = advanceGridTick grid
advanceGridTicks i grid = advanceGridTicks (i - 1) grid >>= advanceGridTick

advanceGridTick :: (RandomGen a) => Grid -> ReaderT Config.Config (Rand a) Grid
advanceGridTick grid = do
  featureConfigs <- ask
  lift $ spreadFeatures featureConfigs grid

spreadFeatures :: (RandomGen a) => [Config.FeatureConfig] -> Grid -> Rand a Grid
spreadFeatures [] grid = return grid
spreadFeatures (feature:otherFeatures) grid = spreadFeature feature grid >>= spreadFeatures otherFeatures

spreadFeature :: (RandomGen a) => Config.FeatureConfig -> Grid -> Rand a Grid
spreadFeature featureConfig grid = do
  let bounds = Array.bounds grid
      (_, pSpread) = Config.growth featureConfig
  assocs <- mapM (spreadFeatureTile featureConfig grid) (Array.assocs grid)
  return $ if (pSpread > 0) 
           then Array.array bounds assocs
           else grid

spreadFeatureTile :: (RandomGen a) => Config.FeatureConfig -> Grid -> ((Int, Int), Tile) -> Rand a ((Int, Int), Tile)
spreadFeatureTile featureConfig grid gridVal@(coords, tile) = do
  let adjacentTiles = getAdjacentTiles grid gridVal
      feature = Config.toFeature featureConfig
  spread <- shouldFeatureSpread featureConfig tile adjacentTiles
  let nextTile =
        if spread
        then tile {feature=Just feature}
        else tile
  return (coords, nextTile)

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

shouldFeatureSpread :: (RandomGen g) => Config.FeatureConfig -> Tile -> [Tile] -> Rand g Bool
shouldFeatureSpread featureConfig currentTile adjacentTiles = do
  p <- getRandom
  let currentTemp = temperature currentTile
      (_, pSpread) = Config.growth featureConfig
      (minTemp, maxTemp) = Config.temperature featureConfig
      currentFeature = Config.toFeature featureConfig
      adjacentTile = any (\tile -> feature tile == Just currentFeature) adjacentTiles
  return (adjacentTile && p < pSpread && currentTemp > minTemp && currentTemp < maxTemp)
