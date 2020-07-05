module MapGen.Models.Feature (
  Feature (..)
) where

data Feature = Feature {
  name :: String
} deriving (Show, Eq, Ord)
