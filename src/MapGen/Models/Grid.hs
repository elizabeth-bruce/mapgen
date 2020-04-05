module MapGen.Models.Grid (
 Grid (..),
 GridCoordinate,
 GridEntry (..)
) where

import Data.Array
import MapGen.Models.Tile

type Grid a = Array (Int, Int) a
type GridCoordinate = (Int, Int) 
type GridEntry a = (GridCoordinate, a)
