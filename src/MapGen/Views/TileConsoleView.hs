module MapGen.Views.TileConsoleView (
  renderTileTemperature,
  renderTileTerrain,
  renderTilePrecipitation
) where

import MapGen.Models.Tile (Tile (..))
import MapGen.Models.Terrain (Terrain (..))

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

renderTileTemperature :: Tile -> String
renderTileTemperature = renderTemperature . temperature

renderTileTerrain :: Tile -> String
renderTileTerrain tile = case terrain tile of
  Plains -> "\x1b[32;1m."
  Ocean -> "\x1b[34;1m~"
  Shallows -> "\x1b[36m~"
  Forest -> "\x1b[32m♣"
  Hills -> "\x1b[32;1mᴖ"
  Mountains -> "\x1b[31;1m▲"
  Peaks -> "\x1b[30;1m▲"
  Aerie -> "\x1b[35m▲"
