{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Discovery.Gen
  where

import Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Gen ()
import Language.Marlowe.Runtime.Discovery.Api
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Instances ()

instance Arbitrary ContractHeader where
  arbitrary = ContractHeader
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
