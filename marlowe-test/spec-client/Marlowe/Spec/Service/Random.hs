

module Marlowe.Spec.Service.Random
  ( generateValue
  ) where


import Data.Jsonable (generateJsonable)
import Marlowe.Spec.Service.Serialization (knownJsonTypes)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck (generate)

import qualified Data.Aeson as A (Value)


generateValue
  :: String
  -> IO (Either String A.Value)
generateValue =
  either (pure . Left) (fmap Right . generate)
    . generateJsonable knownJsonTypes
