module MapGen.Models.Grid (
 Grid (..)
) where

import Data.Array.IArray
import MapGen.Models.Tile

type Grid = Array (Int, Int) Tile
