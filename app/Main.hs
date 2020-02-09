module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Random (newStdGen)

import MapGen.BoundUseCases (boundCreateConsoleViewMap)
import MapGen.Data.Config (Config, getFeatureConfigRaw, parseFeatureConfig)

main :: IO ()
main = do
  g <- newStdGen
  [widthRaw, heightRaw] <- getArgs
  configResult <- parseFeatureConfig <$> getFeatureConfigRaw :: IO (Either String Config)
  let config = case configResult of Left err -> error err
                                    Right val -> val
  let width = read widthRaw :: Int
  let height = read heightRaw :: Int
  putStr $ boundCreateConsoleViewMap g width height config
