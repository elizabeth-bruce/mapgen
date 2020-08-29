module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Random (newStdGen)

import MapGen.BoundUseCases (boundCreateConsoleViewMap)
import MapGen.Data.Config (Config, getConfigRaw, parseConfig)

main :: IO ()
main = do
  g <- newStdGen
  configResult <- parseConfig <$> getConfigRaw :: IO (Either String Config)
  let config = case configResult of Left err -> error err
                                    Right val -> val
  putStr $ boundCreateConsoleViewMap g config
