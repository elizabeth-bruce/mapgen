module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Random (newStdGen)

import MapGen.BoundUseCases (boundCreateConsoleViewMap)

main :: IO ()
main = do
  g <- newStdGen
  [widthRaw, heightRaw] <- getArgs
  let width = read widthRaw :: Int
  let height = read heightRaw :: Int
  putStr $ boundCreateConsoleViewMap g width height
