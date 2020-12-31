module MapGen.UseCases.JsonViewMap (
  jsonViewMap
) where

import MapGen.Models.Map (Map (..))

jsonViewMap :: (Map -> String) -> Map -> String

jsonViewMap viewMap = viewMap
