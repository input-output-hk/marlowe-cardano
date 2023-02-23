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
import Spec.Marlowe.Service.Types (Seed(..), Size(..))
import Test.QuickCheck (generate, variant)
import Test.QuickCheck.Gen (Gen(..))

import qualified Data.Aeson as A (Value)


-- | Generate an arbitrary value.
generateValue
  :: Maybe Size  -- ^ The optional size paramater.
  -> Maybe Seed  -- ^ The optional seed for the generator.
  -> String  -- ^ The key for the type.
  -> IO (Either String A.Value)  -- ^ Either the value or an error message.
generateValue size seed =
  mapM (generate . resize' size . variant' seed) . generateJsonable knownJsonTypes
  where
    variant' :: Maybe Seed -> Gen a -> Gen a
    variant' (Just (Seed n)) = variant n
    variant' Nothing = id

    resize' :: Maybe Size -> Gen a -> Gen a
    resize' (Just (Size n)) (MkGen g) = MkGen (\r _ -> g r n)
    resize' _ g = g
