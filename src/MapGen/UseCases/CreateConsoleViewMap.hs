module MapGen.UseCases.CreateConsoleViewMap (
  createConsoleViewMap
) where

import MapGen.Models.Map (Map (..))

import MapGen.UseCases.CreateMap (createMap)
import MapGen.UseCases.ConsoleViewMap (consoleViewMap)

createConsoleViewMap :: Map -> (Map -> String) -> String

createConsoleViewMap map consoleViewMap =
  consoleViewMap map
