module MapGen.Data.MapTransformer (
  advanceMapTick,
  advanceMapTicks
) where

import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Random (liftRand, Rand (..), Random (..), getRandoms)
import Control.Monad.Trans (lift)
import System.Random (RandomGen)

import Data.Array (assocs, (//), (!))
import Data.Set as S (elems)

import MapGen.Data.Config as Config (Config (..), toFeature)

import MapGen.Models.Feature (Feature (..))
import MapGen.Models.FeatureMap (FeatureMap (..), createFeatureMap, addFeature, addCoordinateToFeature, deleteCoordinateFromFeature, getCoordinatesFromFeatureCoordinateMap)
import MapGen.Models.Map (Map (..))
import MapGen.Models.Grid (Grid (..), GridCoordinate, GridEntry)
import MapGen.Models.Tile (Tile (..))

import MapGen.Data.GridBuilder(createGrid)
import MapGen.Data.Config (Config(..), FeatureConfig (..), toFeature)

advanceMapTicks :: (RandomGen g) => Int -> Map -> ReaderT Config.Config (Rand g) Map
advanceMapTicks 0 m = lift $ return m
advanceMapTicks t m = advanceMapTick m >>= (advanceMapTicks $ t - 1)

advanceMapTick :: (RandomGen g) => Map -> ReaderT Config.Config (Rand g) Map
advanceMapTick m = do
  featureConfigs <- ask
  lift $ foldl (\cm fc -> cm >>= updateFeature fc) (return m) featureConfigs

updateFeature :: (RandomGen g) => FeatureConfig -> Map -> Rand g Map
updateFeature fc Map{featureMap=fm, grid=g} = do
  nextGridCoordinates <- getNextGridCoordinates fc fm
  let feature = toFeature fc
  let updatedFeatureMap = foldl (\cfm gc -> addCoordinateToFeature feature gc cfm) fm nextGridCoordinates
  let updatedTiles = getUpdatedTiles g feature nextGridCoordinates
  let updatedGrid = g // updatedTiles
  return Map{grid=updatedGrid, featureMap=updatedFeatureMap}

getNextGridCoordinates :: (RandomGen g) => FeatureConfig -> FeatureMap -> Rand g [GridCoordinate]
getNextGridCoordinates fc (fcm, _, ((xMin, yMin), (xMax, yMax))) = do
  let gcs = case getCoordinatesFromFeatureCoordinateMap (toFeature fc) fcm of
                 Just set -> S.elems set
                 Nothing -> []
  let adjacentGcs = concat $ map getAdjacentGridCoordinates gcs
  let filteredAdjacentGcs = filter (\(x, y) -> x >= xMin && x < xMax && y >= yMin && y < yMax) adjacentGcs

  randVals <- getRandoms
  let truncatedRandVals = take (length filteredAdjacentGcs) randVals
  let gcsWithRandVals = zip filteredAdjacentGcs truncatedRandVals

  let filteredGcs = filter (\(_, v) -> v < snd (growth fc)) gcsWithRandVals
  return $ map fst filteredGcs

getUpdatedTiles :: Grid Tile -> Feature -> [GridCoordinate] -> [GridEntry Tile]
getUpdatedTiles g f gcs =
  let currentTileVals = map (g !) gcs
      updatedTileVals = map (\t -> t{feature=Just f}) currentTileVals
      updatedTiles = zip gcs updatedTileVals
  in updatedTiles

getAdjacentGridCoordinates :: GridCoordinate -> [GridCoordinate]
getAdjacentGridCoordinates (x, y) =
  [ (x - 1, y)
  , (x + 1, y)
  , (x, y - 1)
  , (x, y + 1)
  ]
