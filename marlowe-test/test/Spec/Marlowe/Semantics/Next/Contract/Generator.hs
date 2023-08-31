{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marlowe.Semantics.Next.Contract.Generator (
  anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds,
  anyCaseContractsWithChoiceOnlyNotShadowed,
  anyCaseContractsWithEmptyBoundsChoiceOnly,
  anyCaseContractsWithIdenticalEvaluatedDeposits,
  anyCaseContractsWithoutIdenticalEvaluatedDeposits,
  anyCloseOrReducedToAClose,
  anyIrreducibleContract,
  anyOnlyFalsifiedNotifies,
  anyReducibleContract,
  anyWithAtLeastOneNotifyTrue,
) where

import Language.Marlowe.Core.V1.Semantics (evalObservation)
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Choice, Deposit, Notify),
  Bound (Bound),
  Case (Case),
  ChoiceId (ChoiceId),
  Contract (Close),
  Environment,
  State,
 )
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck (Arbitrary (arbitrary), Gen, listOf, listOf1, shuffle, suchThat)

import Data.List (nub)
import Spec.Marlowe.Semantics.Next.Common.Tuple (uncurry3)
import Spec.Marlowe.Semantics.Next.Contract (hasValidEnvironement, isIrreducible, isReducible, isReducibleToClose)
import Spec.Marlowe.Semantics.Next.Contract.When.Deposit (hasNoIdenticalEvaluatedDeposits)

anyReducibleContract :: Gen (Environment, State, Contract)
anyReducibleContract =
  anyContract `suchThat` uncurry3 isReducible

anyIrreducibleContract :: Gen (Environment, State, Contract)
anyIrreducibleContract =
  anyContract `suchThat` uncurry3 isIrreducible

anyCloseOrReducedToAClose :: Gen (Environment, State, Contract)
anyCloseOrReducedToAClose =
  anyContract `suchThat` uncurry3 isReducibleToClose

anyContract :: Gen (Environment, State, Contract)
anyContract =
  arbitrary `suchThat` uncurry3 hasValidEnvironement

anyOnlyFalsifiedNotifies :: Gen (Environment, State, [Case Contract])
anyOnlyFalsifiedNotifies =
  do
    (env, state) <- arbitrary
    let genObservationFalse = arbitrary `suchThat` (not . evalObservation env state)
        genFalseNotify = Notify <$> genObservationFalse
    caseContracts <- listOf1 (Case <$> genFalseNotify <*> arbitrary)
    return (env, state, caseContracts)

anyWithAtLeastOneNotifyTrue :: Gen (Environment, State, [Case Contract])
anyWithAtLeastOneNotifyTrue =
  do
    (env, state) <- arbitrary
    let genObservationFalse = arbitrary `suchThat` (not . evalObservation env state)
        genFalseNotify = Notify <$> genObservationFalse
        genObservationTrue = arbitrary `suchThat` evalObservation env state
        genTrueNotify = Notify <$> genObservationTrue
    caseContractsTrue <- listOf1 (Case <$> genTrueNotify <*> arbitrary)
    caseContractsFalse <- listOf (Case <$> genFalseNotify <*> arbitrary)
    caseContract <- shuffle $ caseContractsTrue ++ caseContractsFalse
    return (env, state, caseContract)

anyCaseContractsWithChoiceOnlyNotShadowed :: Gen (Environment, State, [Case Contract])
anyCaseContractsWithChoiceOnlyNotShadowed =
  do
    (env, state) <- arbitrary
    choiceIds <- nub <$> listOf1 arbitrary
    caseContracts <- sequence $ (\choiceId -> pure (Case (Choice choiceId [Bound 1 10])) <*> pure Close) <$> choiceIds
    return (env, state, caseContracts)

anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds :: Gen (Environment, State, [Case Contract])
anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds =
  do
    (env, state) <- arbitrary
    caseContracts <- listOf $ Case <$> (Choice (ChoiceId "same1" "same2") <$> listOf1 arbitrary) <*> pure Close
    return (env, state, caseContracts)

anyCaseContractsWithEmptyBoundsChoiceOnly :: Gen (Environment, State, [Case Contract])
anyCaseContractsWithEmptyBoundsChoiceOnly =
  do
    env <- arbitrary
    state <- arbitrary
    caseContracts <- listOf $ Case <$> (Choice <$> arbitrary <*> pure []) <*> pure Close
    return (env, state, caseContracts)

anyCaseContractsWithoutIdenticalEvaluatedDeposits :: Gen (Environment, State, [Case Contract])
anyCaseContractsWithoutIdenticalEvaluatedDeposits =
  arbitrary `suchThat` uncurry3 hasNoIdenticalEvaluatedDeposits

anyCaseContractsWithIdenticalEvaluatedDeposits :: Gen (Environment, State, [Case Contract])
anyCaseContractsWithIdenticalEvaluatedDeposits =
  do
    env <- arbitrary
    state <- arbitrary
    let genDeposit = Deposit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    caseContracts <- listOf1 (Case <$> genDeposit <*> arbitrary)
    caseContractsWithDuplicates <- shuffle (caseContracts ++ caseContracts ++ caseContracts)
    return (env, state, caseContractsWithDuplicates)
