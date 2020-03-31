module MapGen.Views.TileConsoleView (
  renderTileTemperature,
  renderTileTerrain,
  renderTilePrecipitation
) where

import MapGen.Models.Tile (Tile (..), HeightType (..), TemperatureType (..), PrecipitationType (..))
import MapGen.Models.Feature(Feature (..))

import Debug.Trace

inRange :: Float -> Float -> Float -> Float
inRange min max val = maximum [min, minimum [max, val]]

inRangeColor :: Float -> Float
inRangeColor = inRange 0 255

renderTemperature :: Float -> String
renderTemperature temp =
  let r = show $ floor $ inRangeColor $ temp * 7.5
      g = show 0
      b = show $ floor $ inRangeColor $ 255 - temp * 2.5
  in "\x1b[38;2;" ++ r ++ ";" ++ g ++ ";" ++ b ++ "m█"

renderPrecipitation :: Float -> String
renderPrecipitation precipitation =
  let r = show $ floor $ inRangeColor $ 255 - precipitation * 7.5
      g = show $ floor $ inRangeColor $ 255 - precipitation * 7.5
      b = show $ floor $ inRangeColor 255
  in "\x1b[38;2;" ++ r ++ ";" ++ g ++ ";" ++ b ++ "m█"

renderTilePrecipitation :: Tile -> String
renderTilePrecipitation = renderPrecipitation . precipitation

tileIn :: Tile -> [HeightType] -> [TemperatureType] -> [PrecipitationType] -> Bool
tileIn Tile{temperatureType=t, heightType=h, precipitationType=p} hs ts ps =
  elem h hs && elem t ts && elem p ps

renderTileTemperature :: Tile -> String
renderTileTemperature = renderTemperature . temperature
renderTileFromTerrainType :: Tile -> String
renderTileFromTerrainType tile
  | thisTileIn [Ocean .. Shallows] [Polar] [Minimal .. Extreme] = "\x1b[37;1m."
  | thisTileIn [Ocean] [Polar .. Tropical] [Minimal .. Extreme] = "\x1b[34;1m~"
  | thisTileIn [Shallows] [Polar .. Tropical] [Minimal .. Extreme] = "\x1b[36m~"
  | thisTileIn [Plains] [Polar] [Minimal .. Extreme] = "\x1b[37;1m."
  | thisTileIn [Plains] [Subpolar .. Tropical] [Minimal .. Low] = "\x1b[33;1m."
  | thisTileIn [Plains] [Subpolar .. Tropical] [Medium .. Extreme] = "\x1b[32;1m."
  | thisTileIn [Hills] [Polar] [Minimal .. Extreme] = "\x1b[37;1mᴖ"
  | thisTileIn [Hills] [Subpolar .. Tropical] [Minimal .. Low] = "\x1b[33;1mᴖ"
  | thisTileIn [Hills] [Subpolar .. Tropical] [Medium .. Extreme] = "\x1b[32;1mᴖ"
  | thisTileIn [Mountains] [Polar .. Subpolar] [Minimal .. Extreme] = "\x1b[37;1m▲"
  | thisTileIn [Mountains] [Temperate .. Tropical] [Minimal .. Extreme] = "\x1b[31;1m▲"
  | thisTileIn [Peaks] [Polar .. Tropical] [Minimal .. Extreme] = "\x1b[30;1m▲"
  | otherwise = "X"
  where thisTileIn :: [HeightType] -> [TemperatureType] -> [PrecipitationType] -> Bool
        thisTileIn = tileIn tile

renderTileFromFeature :: Feature -> String
renderTileFromFeature feature = case name feature of
  "Forest" -> "\x1b[32m♣"
  "Aerie" -> "\x1b[35m▲"

renderTileTerrain :: Tile -> String
renderTileTerrain tile = case feature tile of
  Just feature -> renderTileFromFeature feature
  Nothing -> renderTileFromTerrainType tile
