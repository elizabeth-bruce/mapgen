module MapGen.Models.Terrain (
  Terrain (..)
) where

import System.Random (Random, random, randomR)

data Terrain = Plains | Shallows | Ocean | Forest | Hills | Mountains | Peaks deriving (Show, Eq, Ord, Enum, Bounded)

instance Random Terrain where
  randomR (lo, hi) g =
    case randomR (fromEnum lo, fromEnum hi) g of
    (val, g') -> (toEnum val, g')
  random = randomR (minBound, maxBound)
