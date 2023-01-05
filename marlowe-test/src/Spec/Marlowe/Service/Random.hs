

module Spec.Marlowe.Service.Random
  ( generateValue
  ) where


import Data.Jsonable (generateJsonable)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Service.Serialization (knownJsonTypes)
import Test.QuickCheck (generate)

import qualified Data.Aeson as A (Value)


generateValue
  :: String
  -> IO (Either String A.Value)
generateValue =
  either (pure . Left) (fmap Right . generate)
    . generateJsonable knownJsonTypes
