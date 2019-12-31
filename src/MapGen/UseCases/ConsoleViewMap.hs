module MapGen.UseCases.ConsoleViewMap (
  consoleViewMap
) where

import MapGen.Models.Map (Map (..))

consoleViewMap :: (Map -> String) -> Map -> String

consoleViewMap viewMap = viewMap
