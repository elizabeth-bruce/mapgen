{-# LANGUAGE OverloadedStrings #-}

module MapGen.Data.Config (getFeatureConfigRaw, parseFeatureConfig, Config, FeatureConfig (..)) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Data.Text.Lazy (Text, fromStrict, unpack)
import qualified Data.HashMap.Lazy as HML (lookup)
import Control.Applicative (empty, pure)
import MapGen.Models.Terrain ( Terrain (..))

import Text.Read (readEither)

type Config = [FeatureConfig]

data FeatureConfig = FeatureConfig {
  terrain :: Terrain,
  temperature :: (Float, Float),
  height :: (Float, Float),
  growth :: (Float, Float)
}

instance FromJSON Terrain where
    parseJSON (String s) =
      let fromString :: String -> Parser Terrain
          fromString terrain =
            let terrainResult = readEither terrain
            in case terrainResult of Left _ -> empty
                                     Right val -> pure val
      in fromString $ unpack $ fromStrict s

instance FromJSON FeatureConfig where
  parseJSON = withObject "FeatureConfig" $ \v -> FeatureConfig
    <$> v .: "terrain"
    <*> v .: "temperature"
    <*> v .: "height"
    <*> v .: "growth"

parseFeatureConfig :: B.ByteString -> Either String [FeatureConfig]
parseFeatureConfig = eitherDecode

configFilePath :: FilePath
configFilePath = "config/features.json"

getFeatureConfigRaw :: IO B.ByteString
getFeatureConfigRaw = B.readFile configFilePath
