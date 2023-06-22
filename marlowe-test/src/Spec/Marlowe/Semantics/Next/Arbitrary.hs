
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Semantics.Next.Arbitrary
  (
  ) where


import Test.QuickCheck (Arbitrary(..))

import Spec.Marlowe.Semantics.Arbitrary ()

import Language.Marlowe.Core.V1.Next (Next(Next))

import Language.Marlowe.Core.V1.Next.Applicables.CanChoose (CanChoose(CanChoose))
import Language.Marlowe.Core.V1.Next.Applicables.CanDeposit (CanDeposit(..))
import Language.Marlowe.Core.V1.Next.Applicables.CanNotify (CanNotify(..))
import Language.Marlowe.Core.V1.Next.Indexed (CaseIndex(CaseIndex), Indexed(..))
import Language.Marlowe.Core.V1.Next.IsMerkleizedContinuation (IsMerkleizedContinuation(IsMerkleizedContinuation))

import Language.Marlowe.Core.V1.Next.Applicables (ApplicableInputs(ApplicableInputs))
import Language.Marlowe.Core.V1.Next.CanReduce (CanReduce(CanReduce))


instance Arbitrary Next where
  arbitrary = Next . CanReduce <$> arbitrary <*> arbitrary

instance Arbitrary  CaseIndex where
  arbitrary = CaseIndex <$> arbitrary

instance Arbitrary ApplicableInputs where
  arbitrary =
    ApplicableInputs <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Indexed CanNotify) where
  arbitrary = Indexed <$> (CaseIndex <$> arbitrary ) <*> (CanNotify <$> (IsMerkleizedContinuation <$> arbitrary ))

instance Arbitrary (Indexed CanDeposit) where
  arbitrary = Indexed <$> (CaseIndex <$> arbitrary ) <*> (CanDeposit <$>  arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary <*> (IsMerkleizedContinuation <$> arbitrary ))

instance Arbitrary  (Indexed CanChoose) where
  arbitrary = Indexed <$> (CaseIndex <$> arbitrary ) <*> (CanChoose <$>  arbitrary <*> arbitrary <*> (IsMerkleizedContinuation <$> arbitrary ))
