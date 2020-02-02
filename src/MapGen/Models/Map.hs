module MapGen.Models.Map (
  Map (..)
) where

import MapGen.Models.Grid (Grid (..))

newtype Map = Map { grid :: Grid } deriving (Show, Eq)
