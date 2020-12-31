module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Random (newStdGen)

import MapGen.BoundUseCases (boundCreateConsoleViewMap, boundCreateJsonViewMap)
import MapGen.Data.Config (Config, getConfigRaw, parseConfig)

import qualified Data.ByteString.Lazy as B (putStr)

main :: IO ()
main = do
  g <- newStdGen
  configResult <- parseConfig <$> getConfigRaw :: IO (Either String Config)
  let config = case configResult of Left err -> error err
                                    Right val -> val
  B.putStr $ boundCreateJsonViewMap g config
