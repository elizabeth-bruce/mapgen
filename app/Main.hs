module Main where

import System.Random (newStdGen)
import MapGen.BoundUseCases (boundCreateConsoleViewMap)

main :: IO ()
main = do
  g <- newStdGen
  putStr $ boundCreateConsoleViewMap g 250 65
