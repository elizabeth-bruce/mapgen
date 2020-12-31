module MapGen.UseCases.CreateJsonViewMap (
  createJsonViewMap
) where

import qualified Data.ByteString.Lazy as B (ByteString (..))

import MapGen.Models.Map (Map (..))

createJsonViewMap :: Map -> (Map -> B.ByteString) -> B.ByteString

createJsonViewMap map jsonViewMap =
  jsonViewMap map                                
