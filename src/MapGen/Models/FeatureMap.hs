module MapGen.Models.FeatureMap(
  FeatureMap (..),
  createFeatureMap,
  addFeature,
  addCoordinateToFeature,
  getCoordinatesFromFeatureCoordinateMap,
  deleteCoordinateFromFeature
) where

import qualified Data.Map as M (Map (..), adjust, delete, empty, insert, lookup, map)
import qualified Data.Set as S (Set (..), delete, empty, insert, fromList, singleton)

import MapGen.Models.Feature (Feature (..))
import MapGen.Models.Grid (GridCoordinate)

import Debug.Trace (traceShowId, traceShow)

type CoordinateSet = S.Set GridCoordinate
type FeatureCoordinateMap = M.Map Feature CoordinateSet
type CoordinateFeatureMap = M.Map GridCoordinate Feature
type Bounds = (GridCoordinate, GridCoordinate)
type FeatureMap = (FeatureCoordinateMap, CoordinateFeatureMap, Bounds)

createFeatureCoordinateMap :: FeatureCoordinateMap
createFeatureCoordinateMap = M.empty

createCoordinateFeatureMap :: CoordinateFeatureMap
createCoordinateFeatureMap = M.empty

createFeatureMap :: Int -> Int -> FeatureMap
createFeatureMap width height = (createFeatureCoordinateMap, createCoordinateFeatureMap, ((0,0), (width, height)))

addCoordinateToFeatureCoordinateMap :: Feature -> GridCoordinate -> FeatureCoordinateMap -> FeatureCoordinateMap
addCoordinateToFeatureCoordinateMap f gc fcm =
  case M.lookup f fcm of
       Just _ -> M.adjust (S.insert gc) f fcm
       Nothing -> M.insert f (S.singleton gc) fcm

deleteCoordinateFromFeatureCoordinateMap :: Feature -> GridCoordinate -> FeatureCoordinateMap -> FeatureCoordinateMap
deleteCoordinateFromFeatureCoordinateMap f gc fcm = M.adjust (S.delete gc) f fcm

deleteCoordinateFromCoordinateFeatureMap :: GridCoordinate -> CoordinateFeatureMap -> CoordinateFeatureMap
deleteCoordinateFromCoordinateFeatureMap = M.delete

getFeatureFromCoordinateFeatureMap :: GridCoordinate -> CoordinateFeatureMap -> Maybe Feature
getFeatureFromCoordinateFeatureMap = M.lookup

getCoordinatesFromFeatureCoordinateMap :: Feature -> FeatureCoordinateMap -> Maybe (S.Set GridCoordinate)
getCoordinatesFromFeatureCoordinateMap = M.lookup

updateCoordinateFeatureMap :: GridCoordinate -> Feature -> CoordinateFeatureMap -> CoordinateFeatureMap
updateCoordinateFeatureMap = M.insert

updateFeatureMapCoordinate :: GridCoordinate -> Feature -> FeatureMap -> FeatureMap
updateFeatureMapCoordinate gc f (fcm, cfm, bounds) =
  case getFeatureFromCoordinateFeatureMap gc cfm of
    Nothing -> (addCoordinateToFeatureCoordinateMap f gc fcm, updateCoordinateFeatureMap gc f cfm, bounds)
    Just ef -> let newFeatureCoordinateMap = addCoordinateToFeatureCoordinateMap f gc (deleteCoordinateFromFeatureCoordinateMap ef gc fcm)
               in (newFeatureCoordinateMap, updateCoordinateFeatureMap gc f cfm, bounds)

addFeature :: Feature -> [GridCoordinate] -> FeatureMap -> FeatureMap
addFeature f gcs fm = foldl (\efm gc -> updateFeatureMapCoordinate gc f efm) fm gcs

addCoordinateToFeature :: Feature -> GridCoordinate -> FeatureMap -> FeatureMap
addCoordinateToFeature f gc fm = updateFeatureMapCoordinate gc f fm

deleteCoordinateFromFeature :: Feature -> GridCoordinate -> FeatureMap -> FeatureMap
deleteCoordinateFromFeature f gc (fcm, cfm, bounds) = (deleteCoordinateFromFeatureCoordinateMap f gc fcm, deleteCoordinateFromCoordinateFeatureMap gc cfm, bounds)
