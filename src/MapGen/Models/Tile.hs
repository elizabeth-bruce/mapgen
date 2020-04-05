module MapGen.Models.Tile (
  HeightType (..),
  TemperatureType (..),
  PrecipitationType (..),
  Tile (..),
  createTile
) where

import MapGen.Models.Feature (Feature (..))

data HeightType = Ocean | Shallows | Plains | Hills | Mountains | Peaks deriving (Enum, Eq, Show, Ord, Bounded)
data TemperatureType = Polar | Subpolar | Temperate | Subtropical | Tropical deriving (Enum, Eq, Show, Ord, Bounded)
data PrecipitationType = Minimal | Low | Medium | High | Extreme deriving (Enum, Eq, Show, Ord, Bounded)

data Tile = Tile {
  height :: Float
  , temperature :: Float
  , precipitation :: Float
  , heightType :: HeightType
  , temperatureType :: TemperatureType
  , precipitationType :: PrecipitationType
  , feature :: Maybe Feature
} deriving (Show, Eq)

getHeightType :: Float -> HeightType
getHeightType h
  | h <= -50 = Ocean
  | h <= 0 = Shallows
  | h < 100 = Plains
  | h < 200 = Hills
  | h < 300 = Mountains
  | otherwise = Peaks

getTemperatureType :: Float -> TemperatureType
getTemperatureType t
  | t < 0 = Polar
  | t < 10 = Subpolar
  | t < 18.5 = Temperate
  | t < 25.0 = Subtropical
  | otherwise = Tropical

getPrecipitationType p
  | p < 10 = Minimal
  | p < 30 = Low
  | p < 50 = Medium
  | p < 100 = High
  | otherwise = Extreme

createTile h t p = Tile{
  height=h
  ,temperature=t
  ,precipitation=p
  ,heightType=getHeightType h
  ,temperatureType=getTemperatureType t
  ,precipitationType=getPrecipitationType p
  ,feature=Nothing
}

getTileDescription :: Tile -> String
getTileDescription = getTileDescriptionFromTerrainType

tileIn :: Tile -> [HeightType] -> [TemperatureType] -> [PrecipitationType] -> Bool
tileIn Tile{temperatureType=t, heightType=h, precipitationType=p} hs ts ps =
  elem h hs && elem t ts && elem p ps

getTileDescriptionFromTerrainType :: Tile -> String
getTileDescriptionFromTerrainType tile
  | thisTileIn [Ocean .. Shallows] [Polar] [Minimal .. Extreme] = "Polar Icecap"
  | thisTileIn [Plains .. Hills] [Polar] [Minimal .. Extreme] = "Tundra"
  | thisTileIn [Mountains .. Peaks] [Polar] [Minimal .. Extreme] = "Polar Mountains"
  | thisTileIn [Ocean] [Tropical .. Subpolar] [Minimal .. Extreme] = "Ocean"
  | thisTileIn [Shallows] [Tropical .. Subpolar] [Minimal .. Extreme] = "Shallows"
  | thisTileIn [Plains .. Hills] [Subpolar] [Minimal .. Low] = "Subpolar Desert"
  | thisTileIn [Plains .. Hills] [Subpolar] [Medium] = "Subpolar Steppe"
  | thisTileIn [Plains .. Hills] [Subpolar] [High .. Extreme] = "Subpolar Plains"
  | thisTileIn [Mountains .. Peaks] [Subpolar] [Minimal .. Extreme] = "Subpolar Mountains"
  | thisTileIn [Plains] [Temperate] [Minimal .. Low] = "Temperate Desert"
  | thisTileIn [Plains] [Temperate] [Medium .. Extreme] = "Temperate Plains"
  | thisTileIn [Hills] [Temperate] [Minimal .. Low] = "Temperate Desert"
  | thisTileIn [Hills] [Temperate] [Medium .. Extreme] = "Temperate Hills"
  | thisTileIn [Mountains .. Peaks] [Temperate] [Minimal .. Extreme] = "Temperate Mountains"
  | thisTileIn [Plains] [Subtropical] [Minimal .. Extreme] = "Subtropical Plains"
  | thisTileIn [Hills] [Subtropical] [Minimal .. Extreme] = "Subtropical Hills"
  | thisTileIn [Mountains .. Peaks] [Subtropical] [Minimal .. Extreme] = "Subtropical Mountains"
  | thisTileIn [Plains] [Tropical] [Minimal .. Extreme] = "Tropical Plains"
  | thisTileIn [Hills] [Tropical] [Minimal .. Extreme] = "Tropical Hills"
  | thisTileIn [Mountains .. Peaks] [Tropical] [Minimal .. Extreme] = "Tropical Mountains"
  | otherwise = "TEST"
  where thisTileIn :: [HeightType] -> [TemperatureType] -> [PrecipitationType] -> Bool
        thisTileIn = tileIn tile

