module MapGen.Data.Config (FeatureConfig (..), getFeatureConfigs) where

import MapGen.Models.Terrain ( Terrain (..))

data FeatureConfig = FeatureConfig { terrain :: Terrain, temperature :: (Float, Float), height :: (Float, Float), pGrowth :: (Float, Float) }

getForestConfig :: FeatureConfig

getForestConfig = FeatureConfig{
  terrain=Forest,
  temperature=(0.0, 30.0),
  height=(0.0, 100.0),
  pGrowth=(0.01, 0.05)
}

getAerieConfig = FeatureConfig{
  terrain=Aerie,
  temperature=(0.0, 50.0),
  height=(250, 9000),
  pGrowth=(0.01, 0.0)
}

getFeatureConfigs :: [FeatureConfig]

getFeatureConfigs = [getForestConfig, getAerieConfig]
