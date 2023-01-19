-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Arbitrary instance for an existentially quantified JSON type.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Service.Random
  ( -- * Testing
    generateValue
  ) where


import Data.Jsonable (generateJsonable)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Service.Serialization (knownJsonTypes)
import Test.QuickCheck (generate)

import qualified Data.Aeson as A (Value)


-- | Generate an arbitrary value.
generateValue
  :: String  -- ^ The key for the type.
  -> IO (Either String A.Value)  -- ^ Either the value or an error message.
generateValue =
  either (pure . Left) (fmap Right . generate)
    . generateJsonable knownJsonTypes
