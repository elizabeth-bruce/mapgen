module MapGen.Models.Map (
  Map (..)
) where

import MapGen.Models.Grid (Grid (..))

data Map = Map { grid :: Grid } deriving (Show, Eq)
