module MapGen (
   createMap,
   Map (..),
   Config (..)
) where

import MapGen.Data.Config (Config (..))
import MapGen.BoundUseCases (createMap)
import MapGen.Models.Map (Map (..))
