{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MapGen.Data.Config (
  getConfigRaw
  ,parseConfig
  ,Config (..)
  ,FeatureConfig (..)
  ,MapConfig (..)
  ,FrequencyConfig (..)
  ,toFeature
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import qualified MapGen.Models.Feature as Feature (Feature (..))

data Config = Config {
  mapConfig :: MapConfig,
  featureConfig :: [FeatureConfig]
}

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "mapConfig"
    <*> v .: "featureConfig"

data MapConfig = MapConfig {
  age :: Int
  ,width :: Int
  ,height :: Int
  ,roughness :: Float
  ,frequency :: FrequencyConfig 
}

instance FromJSON MapConfig where
  parseJSON = withObject "MapConfig" $ \v -> MapConfig
    <$> v .: "age"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "roughness"
    <*> v .: "frequency"

data FrequencyConfig = FrequencyConfig {
    decayX :: Float
    ,decayY :: Float
    ,decayXY :: Float
}

instance FromJSON FrequencyConfig where {
  parseJSON = withObject "FrequencyConfig" $ \v ->FrequencyConfig
    <$> v .: "decayX"
    <*> v .: "decayY"
    <*> v .: "decayXY"
}

data FeatureConfig = FeatureConfig {
  name :: String
  ,temperature :: (Float, Float)
  ,precipitation :: (Float, Float)
  ,height :: (Float, Float)
  ,growth :: (Float, Float)
}

instance FromJSON FeatureConfig where
  parseJSON = withObject "FeatureConfig" $ \v -> FeatureConfig
    <$> v .: "name"
    <*> v .: "temperature"
    <*> v .: "precipitation"
    <*> v .: "height"
    <*> v .: "growth"

toFeature :: FeatureConfig -> Feature.Feature
toFeature FeatureConfig{name=name} = Feature.Feature name

parseConfig :: B.ByteString -> Either String Config
parseConfig = eitherDecode

configFilePath :: FilePath
configFilePath = "config/config.json"

getConfigRaw :: IO B.ByteString
getConfigRaw = B.readFile configFilePath
