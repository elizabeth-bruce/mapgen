module MapGen.UseCases.CreateMap (
  createMap
) where

import MapGen.Models.Map (Map (..))

createMap :: Map -> Map

createMap = id
