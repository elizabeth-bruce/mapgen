{-# LANGUAGE OverloadedStrings #-}

module MapGen.Data.Config (
  getFeatureConfigRaw 
  ,parseFeatureConfig 
  ,Config
  ,FeatureConfig (..)
  ,toFeature
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import qualified MapGen.Models.Feature as Feature (Feature (..))

type Config = [FeatureConfig]

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

parseFeatureConfig :: B.ByteString -> Either String [FeatureConfig]
parseFeatureConfig = eitherDecode

configFilePath :: FilePath
configFilePath = "config/features.json"

getFeatureConfigRaw :: IO B.ByteString
getFeatureConfigRaw = B.readFile configFilePath
