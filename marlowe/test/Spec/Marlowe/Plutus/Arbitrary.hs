-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Generate random data for Plutus tests.
--
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Plutus.Arbitrary
where


import Plutus.V1.Ledger.Api (Value (..))
import Spec.Marlowe.Semantics.Arbitrary
import Test.Tasty.QuickCheck (Arbitrary (..))


instance Arbitrary Value where
  arbitrary = Value <$> arbitraryAssocMap arbitrary (arbitraryAssocMap arbitrary arbitrary)
