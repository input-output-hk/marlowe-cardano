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
import Test.QuickCheck (generate)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Random (mkQCGen, newQCGen)

import qualified Data.Aeson as A (Value)


-- | Generate an arbitrary value.
generateValue
  :: Maybe Size  -- ^ The optional size paramater.
  -> Maybe Seed  -- ^ The optional seed for the generator.
  -> String  -- ^ The key for the type.
  -> IO (Either String A.Value)  -- ^ Either the value or an error message.
generateValue size seed =
  mapM (generate' size seed) . generateJsonable knownJsonTypes
  where
    generate' :: Maybe Size -> Maybe Seed -> Gen a -> IO a
    generate' (Just (Size s)) (Just (Seed r)) (MkGen g) = return $ g (mkQCGen r) s
    generate' Nothing (Just (Seed r)) (MkGen g) = return $ g (mkQCGen r) 30
    generate' (Just (Size s)) Nothing (MkGen g) = newQCGen >>= \r -> return $ g r s
    generate' _ _ g = generate g
