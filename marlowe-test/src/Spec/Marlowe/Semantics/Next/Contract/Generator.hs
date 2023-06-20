
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}


module Spec.Marlowe.Semantics.Next.Contract.Generator
  ( anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds
  , anyCaseContractsWithChoiceOnlyNotShadowed
  , anyCaseContractsWithEmptyBoundsChoiceOnly
  , anyCaseContractsWithIdenticalEvaluatedDeposits
  , anyCaseContractsWithoutIdenticalEvaluatedDeposits
  , anyCloseOrReducedToAClose
  , anyEmptyWhenNonTimedOut
  , anyIrreducibleContract
  , anyOnlyFalsifiedNotifies
  , anyReducibleContract
  , anyWithAtLeastOneNotifyTrue
  , anyWithValidEnvironement
  ) where


import Language.Marlowe
  (Action(Choice, Notify), Bound(Bound), Case(Case), ChoiceId(ChoiceId), Contract(Close, When), Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck (Arbitrary(arbitrary), Gen, listOf, listOf1, suchThat)

import Data.List (nub)
import Spec.Marlowe.Semantics.Next.Common.Tuple (uncurry3)
import Spec.Marlowe.Semantics.Next.Contract
  (hasValidEnvironement, isEmptyWhenNotTimedOut, isIrreducible, isReducible, isReducibleToClose)
import Spec.Marlowe.Semantics.Next.Contract.When.Deposit
import Spec.Marlowe.Semantics.Next.Contract.When.Notify (areOnlyFalsifiedNotifies, atLeastOneNotifyTrue)


anyReducibleContract :: Gen (Environment,State,Contract)
anyReducibleContract
  = anyContract
      `suchThat` uncurry3 isReducible


anyIrreducibleContract :: Gen (Environment,State,Contract)
anyIrreducibleContract
  = anyContract
      `suchThat` uncurry3 isIrreducible


anyOnlyFalsifiedNotifies :: Gen (Environment,State,Contract)
anyOnlyFalsifiedNotifies
  = do
      let genOnlyNotifies = When <$> (listOf1 $ Case <$> (Notify <$> arbitrary) <*> arbitrary) <*> arbitrary <*> arbitrary
      env <- arbitrary
      state <- arbitrary
      contract <- genOnlyNotifies `suchThat` areOnlyFalsifiedNotifies env state
      return (env,state,contract)

anyWithAtLeastOneNotifyTrue :: Gen (Environment,State,Contract)
anyWithAtLeastOneNotifyTrue
  = anyContract `suchThat` uncurry3 atLeastOneNotifyTrue

anyCloseOrReducedToAClose :: Gen (Environment,State,Contract)
anyCloseOrReducedToAClose
  = anyContract `suchThat` uncurry3 isReducibleToClose

anyWithValidEnvironement :: Gen (Environment,State,Contract)
anyWithValidEnvironement
  = anyContract `suchThat` uncurry3 hasValidEnvironement


anyCaseContractsWithChoiceOnlyNotShadowed :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithChoiceOnlyNotShadowed
  = do
    env <- arbitrary
    state <- arbitrary
    choiceIds <- nub <$> listOf1 arbitrary
    caseContracts <- sequence $ (\choiceId -> pure (Case (Choice choiceId [Bound 1 10])) <*> pure Close) <$> choiceIds
    return (env,state,caseContracts)


anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds
  = do
    env <- arbitrary
    state <- arbitrary
    caseContracts <- listOf  $ Case <$> (Choice (ChoiceId "same1" "same2") <$> listOf1 arbitrary) <*> pure Close
    return (env,state, caseContracts)

anyCaseContractsWithEmptyBoundsChoiceOnly :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithEmptyBoundsChoiceOnly
  = do
    env <- arbitrary
    state <- arbitrary
    caseContracts <- listOf  $ Case <$> (Choice <$> arbitrary <*> pure []) <*> pure Close
    return (env,state,caseContracts)

anyCaseContractsWithoutIdenticalEvaluatedDeposits :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithoutIdenticalEvaluatedDeposits
  = arbitrary `suchThat`  uncurry3 hasNoIdenticalEvaluatedDeposits

anyCaseContractsWithIdenticalEvaluatedDeposits :: Gen (Environment,State,[Case Contract])
anyCaseContractsWithIdenticalEvaluatedDeposits
  = arbitrary `suchThat` uncurry3 hasIdenticalEvaluatedDeposits

anyContract :: Gen (Environment,State,Contract)
anyContract
  = arbitrary

anyEmptyWhenNonTimedOut :: Gen (Environment,State,Contract)
anyEmptyWhenNonTimedOut
  = arbitrary `suchThat` uncurry3 isEmptyWhenNotTimedOut


