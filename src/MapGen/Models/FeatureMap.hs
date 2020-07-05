module MapGen.Models.FeatureMap(
  FeatureMap (..),
  createFeatureMap,
  addFeature,
  addCoordinateToFeature,
  deleteCoordinateFromFeature
) where

import qualified Data.Map as M (Map (..), adjust, empty, insert, map)
import qualified Data.Set as S (Set (..), delete, empty, insert, fromList)

import MapGen.Models.Feature (Feature (..))
import MapGen.Models.Grid (GridCoordinate)

type CoordinateSet = S.Set GridCoordinate
type FeatureMap = M.Map Feature CoordinateSet

createFeatureMap :: FeatureMap
createFeatureMap = M.empty

addFeature :: Feature -> [GridCoordinate] -> FeatureMap -> FeatureMap
addFeature f gcs fm = M.insert f (S.fromList gcs) fm

addCoordinateToFeature :: Feature -> GridCoordinate -> FeatureMap -> FeatureMap
addCoordinateToFeature f gc fm = M.adjust (S.insert gc) f fm

deleteCoordinateFromFeature :: Feature -> GridCoordinate -> FeatureMap -> FeatureMap
deleteCoordinateFromFeature f gc fm = M.adjust (S.delete gc) f fm
