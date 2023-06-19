{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marlowe.Semantics.Next
  ( tests
  ) where

import Data.Coerce (coerce)
import Data.List (nubBy)
import Data.Maybe (fromJust)
import Data.Types.Isomorphic (Injective(to))


import Language.Marlowe.Core.V1.Semantics.Next (Next(..), next)
import Language.Marlowe.Core.V1.Semantics.Next.Applicables
  (ApplicableInputs(choices, deposits, notifyMaybe), emptyApplicables, mkApplicablesWhen)
import Language.Marlowe.Core.V1.Semantics.Next.Applicables.CanChoose (compactAdjoinedBounds, overlaps)
import Language.Marlowe.Core.V1.Semantics.Next.CanReduce (CanReduce(..))
import Language.Marlowe.Core.V1.Semantics.Next.Indexed (getCaseIndex, sameIndexedValue)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Common.Isomorphism ()
import Spec.Marlowe.Semantics.Next.Common.QuickCheck (forAll')
import Spec.Marlowe.Semantics.Next.Contract.Generator
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
  )
import Spec.Marlowe.Semantics.Next.Contract.When.Choice (onlyIndexedChoices)
import Spec.Marlowe.Semantics.Next.Contract.When.Deposit (evaluateDeposits)
import Spec.Marlowe.Semantics.Next.Contract.When.Notify (firstNotifyTrueIndex)
import Test.QuickCheck (withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Next"
  [ testGroup "Reducibility"
      [ testProperty
          "Can Reduce when contract is reducible"
            $ forAll' anyReducibleContract $ \(environment', state, contract) ->
                Right (coerce True) == (canReduce <$> next environment' state contract)
      , testProperty
          "Can't Reduce when the contract provided is irreducible"
            $ forAll' anyIrreducibleContract $ \(environment', state, contract) ->
                Right (coerce False) == (canReduce <$> next environment' state contract)
      , testProperty
          "Can Reduce a \"Close\""
            $ forAll' anyCloseOrReducedToAClose $ \(environment', state, contract) ->
                Right (coerce True) == (canReduce <$> next environment' state contract)
      ]
  , testGroup "Applicability"
      [ testProperty
          "\"Close\" is not applicable"
            $ forAll' anyCloseOrReducedToAClose $ \(environment', state, contract) ->
                Right emptyApplicables == (applicables <$> next environment' state contract)
      , testProperty
          "Non timed out empty \"When\" is not applicable"
            $ forAll' anyEmptyWhenNonTimedOut $ \(environment', state, contract) ->
                Right emptyApplicables == ( applicables <$> next environment' state contract)
      , testGroup "Notify"
          [ testProperty
              "\"Notify\" is not applicable when evaluated falsified"
                $ forAll' anyOnlyFalsifiedNotifies $ \(environment', state, contract) ->
                    Right Nothing == (notifyMaybe . applicables <$> next environment' state contract)
         , testProperty
              "Shadowing : Following Notifies evaluated to True are not applicable"
                $ forAll' anyWithAtLeastOneNotifyTrue $ \(environment', state, contract) -> do
                    let expectedCaseIndex = fromJust . firstNotifyTrueIndex environment' state $ contract
                    (Right . Just $ expectedCaseIndex ) == ( (getCaseIndex <$>). notifyMaybe . applicables <$> next environment' state contract)
          ]
      , testGroup "Deposit"
          [ testProperty
              "\"CanDeposit\" is a \"Deposit\" with its quantity evaluated and its \"Case\" index preserved (when no shadowing involved)"
                $ forAll' anyCaseContractsWithoutIdenticalEvaluatedDeposits $ \(environment', state, caseContracts) -> do
                    let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                    evaluatedDeposits == (to . deposits. mkApplicablesWhen environment' state $ caseContracts)

          , testProperty
              "Shadowing : Following Identical Evaluated Deposits are not applicable"
                $ forAll' anyCaseContractsWithIdenticalEvaluatedDeposits $ \(environment', state, caseContracts) -> do
                    let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                        canDeposits = to. deposits. mkApplicablesWhen environment' state $ caseContracts
                    canDeposits == nubBy sameIndexedValue evaluatedDeposits
          ]
      , testGroup "Choice"
          [ testProperty
              "\"Choice\" with empty bounds is not applicables"
                $ forAll' anyCaseContractsWithEmptyBoundsChoiceOnly $ \(environment', state, caseContracts) -> do
                    null(choices . mkApplicablesWhen environment' state $ caseContracts)
          , testGroup "Shadowing : Bounds are not applicable when previously covered by Choices with the same Id"
            [ testProperty
                "Applicable [Indexed CanChoose]'s bounds on the same choiceId don't overlap"
                  $ forAll' anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds $ \(environment', state, caseContracts) -> do
                      let indexedChoices =  to . onlyIndexedChoices environment' state $ caseContracts
                          canchooseList = choices . mkApplicablesWhen environment' state $ caseContracts
                      overlaps indexedChoices && (not. overlaps $ canchooseList)
                        || (not. overlaps $ indexedChoices) && (not. overlaps $ canchooseList)
            , testProperty
                "\"CanChoose\" is isomorphic to \"Choice\" and its \"Case\" index preserved when no shadowing involved"
                  $ forAll' anyCaseContractsWithChoiceOnlyNotShadowed $ \(environment', state, caseContracts) -> do
                      let indexedChoices =  onlyIndexedChoices environment' state caseContracts
                      indexedChoices == (to . choices . mkApplicablesWhen environment' state $ caseContracts)
            , testProperty
                "\"[Indexed CanChoose]\" and [Choice] on the same id have the same merged Bounds "
                  $ withMaxSuccess 50 $ forAll' anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds $ \(environment', state, caseContracts) -> do
                      let indexedChoices =  to . onlyIndexedChoices environment' state $ caseContracts
                          canchooseList = choices . mkApplicablesWhen environment' state $ caseContracts
                      compactAdjoinedBounds indexedChoices == compactAdjoinedBounds canchooseList]

          ]
      ]
  ]


