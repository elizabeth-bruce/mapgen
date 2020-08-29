{-# LANGUAGE DuplicateRecordFields #-}

module MapGen.Data.MapBuilder (
  createMap,
  createMapWithFeatures,
  canFeatureGrow
) where

import Control.Monad.Reader (ReaderT (..), ask, liftM)
import Control.Monad.Random (liftRand, Rand (..), Random (..), getRandoms)
import Control.Monad.Trans (lift)
import System.Random (RandomGen)

import Data.Array (assocs, (//))

import MapGen.Models.Feature (Feature (..))
import MapGen.Models.Map (Map (..))
import MapGen.Models.Grid (Grid (..), GridEntry)
import MapGen.Models.Tile (Tile (..))

import MapGen.Models.FeatureMap (FeatureMap (..), createFeatureMap, addFeature, addCoordinateToFeature, deleteCoordinateFromFeature)

import MapGen.Data.GridBuilder(createGrid)
import MapGen.Data.Config (Config(..), FeatureConfig (..), toFeature)

type TileGrid = Grid Tile

createMap :: TileGrid -> FeatureMap -> Map

createMap grid featureMap = Map {grid=grid, featureMap=featureMap}

seedTileWithFeature :: FeatureConfig -> Tile -> Tile
seedTileWithFeature fc t = t{feature=Just $ toFeature fc}

canFeatureGrow :: FeatureConfig -> Tile -> Bool
canFeatureGrow FeatureConfig {
  temperature=(tMin, tMax)
  ,height=(hMin, hMax)
  ,precipitation=(pMin, pMax)
} Tile{
  height=h
  ,temperature=t
  ,precipitation=p
} = t >= tMin && t <= tMax && h >= hMin && h <= hMax && p >= pMin && p <= pMax

seedGridWithFeature :: FeatureConfig -> [GridEntry Tile] -> TileGrid -> TileGrid
seedGridWithFeature fc ges g =
  let updateGridEntry ((x, y), tile) = ((x, y), seedTileWithFeature fc tile)
      updatedGridEntries = map updateGridEntry ges
  in g // updatedGridEntries

seedMapWithFeature :: (RandomGen g) => FeatureConfig -> Map -> Rand g Map
seedMapWithFeature fc Map{grid=g, featureMap=fm} = do
  let candidateGridEntries = filter ((canFeatureGrow fc) . snd) (assocs g)
  gcVals <- (take $ length candidateGridEntries) <$> getRandoms
  let (pSeed, _) = growth fc
      seededGridEntries = map snd $ filter ((< pSeed) . fst) $ zip gcVals candidateGridEntries
      seededGridCoordinates = map fst seededGridEntries
      seededGrid = seedGridWithFeature fc seededGridEntries g
      seededFeatureMap = addFeature (toFeature fc) seededGridCoordinates fm
  return $ createMap seededGrid seededFeatureMap

seedMapWithFeatures :: (RandomGen g) => [FeatureConfig] -> Map -> Rand g Map
seedMapWithFeatures [] m = return m
seedMapWithFeatures fcs m =
  let fc:ofcs = fcs
  in seedMapWithFeature fc m >>= seedMapWithFeatures ofcs

createMapWithFeatures :: (RandomGen g) => Int -> Int -> ReaderT Config (Rand g) Map
createMapWithFeatures width height = do
  config <- ask
  let features = featureConfig config
  let m = createMap <$> createGrid (mapConfig config) <*> pure (createFeatureMap width height)
  lift $ m >>= seedMapWithFeatures features
