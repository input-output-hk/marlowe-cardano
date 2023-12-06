{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Marlowe.Semantics.Next (
  tests,
) where

import Data.Coerce (coerce)
import Data.List (group, nubBy, sort)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.Types.Isomorphic (Injective (to))

import Data.Aeson (ToJSON (..), encode, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Function (on)
import Data.Semigroup (All (..))
import GHC.Generics (Generic)
import Language.Marlowe (
  Case (..),
  Contract (Close),
  Environment (Environment),
  TimeInterval,
  evalObservation,
  evalValue,
 )
import Language.Marlowe.Core.V1.Next (Next (..), next)
import Language.Marlowe.Core.V1.Next.Applicables (
  ApplicableInputs (choices, deposits, notifyMaybe),
  emptyApplicables,
  mkApplicablesWhen,
 )
import Language.Marlowe.Core.V1.Next.Applicables.CanChoose (compactAdjoinedBounds, overlaps)
import Language.Marlowe.Core.V1.Next.CanReduce (CanReduce (..))
import Language.Marlowe.Core.V1.Next.Indexed (getCaseIndex, sameIndexedValue)
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (..),
  State (State, minTime),
  Value (..),
  getAction,
 )
import qualified PlutusLedgerApi.V2 as PV2
import Spec.Marlowe.Semantics.Arbitrary (arbitraryNonnegativeInteger)
import Spec.Marlowe.Semantics.Next.Common.Isomorphism ()
import Spec.Marlowe.Semantics.Next.Common.QuickCheck (forAllSuchThat)
import Spec.Marlowe.Semantics.Next.Common.Tuple (uncurry3)
import Spec.Marlowe.Semantics.Next.Contract (hasValidEnvironment, isIrreducible, isReducible, isReducibleToClose)
import Spec.Marlowe.Semantics.Next.Contract.Generator (
  anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds,
  anyCaseContractsWithChoiceOnlyNotShadowed,
  anyCaseContractsWithEmptyBoundsChoiceOnly,
  anyCaseContractsWithIdenticalEvaluatedDeposits,
  anyOnlyFalsifiedNotifies,
  anyWithAtLeastOneNotifyTrue,
 )
import Spec.Marlowe.Semantics.Next.Contract.When.Choice (onlyIndexedChoices)
import Spec.Marlowe.Semantics.Next.Contract.When.Deposit (evaluateDeposits, hasNoIdenticalEvaluatedDeposits)
import Spec.Marlowe.Semantics.Next.Contract.When.Notify (firstNotifyTrueIndex)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  forAllShrink,
  genericShrink,
  withMaxSuccess,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Next"
    [ testGroup
        "Reducibility"
        [ testProperty
            "Can Reduce when contract is reducible"
            $ forAllSuchThat (getAll . on (<>) (fmap All . uncurry3) hasValidEnvironment isReducible)
            $ \(environment', state, contract) ->
              Right (coerce True) === (canReduce <$> next environment' state contract)
        , testProperty
            "Can't Reduce when the contract provided is irreducible"
            $ forAllSuchThat (getAll . on (<>) (fmap All . uncurry3) hasValidEnvironment isIrreducible)
            $ \(environment', state, contract) ->
              Right (coerce False) === (canReduce <$> next environment' state contract)
        ]
    , testGroup
        "Applicability"
        [ testProperty
            "\"Close\" is not applicable"
            $ forAllSuchThat (getAll . on (<>) (fmap All . uncurry3) hasValidEnvironment isReducibleToClose)
            $ \(environment', state, contract) ->
              Right emptyApplicables === (applicables <$> next environment' state contract)
        , testGroup
            "Notify"
            [ testProperty
                "\"Notify\" is not applicable when evaluated falsified"
                $ forAllShrink anyOnlyFalsifiedNotifies (filter (uncurry3 onlyFalseNotifies) . shrink)
                $ \(environment', state, caseContracts) ->
                  isNothing (notifyMaybe . mkApplicablesWhen environment' state $ caseContracts)
            , testProperty
                "Shadowing : Following Notifies evaluated to True are not applicable"
                $ forAllShrink anyWithAtLeastOneNotifyTrue (filter (uncurry3 atLeastOneTrueNotify) . shrink)
                $ \(environment', state, caseContracts) -> do
                  let expectedCaseIndex = fromJust . firstNotifyTrueIndex environment' state $ caseContracts
                  Just expectedCaseIndex === ((getCaseIndex <$>) . notifyMaybe . mkApplicablesWhen environment' state $ caseContracts)
            ]
        , testGroup
            "Deposit"
            [ testProperty
                "\"CanDeposit\" is a \"Deposit\" with its quantity evaluated and its \"Case\" index preserved (when no shadowing involved)"
                $ forAllSuchThat (uncurry3 hasNoIdenticalEvaluatedDeposits)
                $ \(environment', state, caseContracts) -> do
                  let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                  evaluatedDeposits === (to . deposits . mkApplicablesWhen environment' state $ caseContracts)
            , testProperty
                "Shadowing : Following Identical Evaluated Deposits are not applicable"
                $ forAllShrink anyCaseContractsWithIdenticalEvaluatedDeposits (filter (uncurry3 hasDuplicateDeposits) . shrink)
                $ \(environment', state, caseContracts) -> do
                  let evaluatedDeposits = evaluateDeposits environment' state caseContracts
                      canDeposits = to . deposits . mkApplicablesWhen environment' state $ caseContracts
                  canDeposits === nubBy sameIndexedValue evaluatedDeposits
            ]
        , testGroup
            "Choice"
            [ testProperty
                "\"Choice\" with empty bounds is not applicables"
                $ forAllShrink anyCaseContractsWithEmptyBoundsChoiceOnly shrink
                $ \(environment', state, caseContracts) -> do
                  null (choices . mkApplicablesWhen environment' state $ caseContracts)
            , testGroup
                "Shadowing : Bounds are not applicable when previously covered by Choices with the same Id"
                [ testProperty
                    "Applicable [Indexed CanChoose]'s bounds on the same choiceId don't overlap"
                    $ forAllShrink anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds shrink
                    $ \(environment', state, caseContracts) -> do
                      let indexedChoices = to . onlyIndexedChoices environment' state $ caseContracts
                          canChooseList = choices . mkApplicablesWhen environment' state $ caseContracts
                      overlaps indexedChoices && (not . overlaps $ canChooseList)
                        || (not . overlaps $ indexedChoices) && (not . overlaps $ canChooseList)
                , testProperty
                    "\"CanChoose\" is isomorphic to \"Choice\" and its \"Case\" index preserved when no shadowing involved"
                    $ forAllShrink anyCaseContractsWithChoiceOnlyNotShadowed shrink
                    $ \(environment', state, caseContracts) -> do
                      let indexedChoices = onlyIndexedChoices environment' state caseContracts
                      indexedChoices === (to . choices . mkApplicablesWhen environment' state $ caseContracts)
                , testProperty
                    "\"[Indexed CanChoose]\" and [Choice] on the same id have the same merged Bounds "
                    $ withMaxSuccess 50
                    $ forAllShrink anyCaseContractsWithChoiceOnTheSameChoiceIdAndNonEmptyBounds shrink
                    $ \(environment', state, caseContracts) -> do
                      let indexedChoices = to . onlyIndexedChoices environment' state $ caseContracts
                          canChooseList = choices . mkApplicablesWhen environment' state $ caseContracts
                      compactAdjoinedBounds indexedChoices === compactAdjoinedBounds canChooseList
                ]
            ]
        , testProperty "JSON representation is compatible with Core" propJSON
        ]
    ]

propJSON :: ToApplicableArgs -> Property
propJSON (ToApplicableArgs env state case_) = encode actual === encode expected
  where
    actual = toJSON $ mkApplicablesWhen env state [case_]
    expected =
      object
        [ "deposits"
            .= case getAction case_ of
              Deposit a p t v ->
                pure $
                  object
                    [ "party" .= p
                    , "can_deposit" .= v
                    , "of_token" .= t
                    , "into_account" .= a
                    , "case_index" .= (0 :: Int)
                    , "is_merkleized_continuation" .= False
                    ]
              _ -> []
        , "choices" .= ([] :: [Aeson.Value])
        ]

data ToApplicableArgs = ToApplicableArgs Environment State (Case Contract)
  deriving (Eq, Show, Generic)

instance Arbitrary ToApplicableArgs where
  arbitrary = do
    state <- arbitrary
    interval <- genTimeInterval state
    let env = Environment interval
    action <- Deposit <$> arbitrary <*> arbitrary <*> arbitrary <*> (Constant <$> arbitrary)
    pure $ ToApplicableArgs env state $ Case action Close
  shrink = genericShrink

genTimeInterval :: State -> Gen TimeInterval
genTimeInterval State{..} = do
  dStart <- arbitraryNonnegativeInteger
  let start = PV2.getPOSIXTime minTime + dStart
  duration <- arbitraryNonnegativeInteger
  pure (PV2.POSIXTime start, PV2.POSIXTime $ start + duration)

onlyFalseNotifies :: Environment -> State -> [Case Contract] -> Bool
onlyFalseNotifies env state = not . any (isTrueNotify env state . getAction)

atLeastOneTrueNotify :: Environment -> State -> [Case Contract] -> Bool
atLeastOneTrueNotify env state = any (isTrueNotify env state . getAction)

hasDuplicateDeposits :: Environment -> State -> [Case Contract] -> Bool
hasDuplicateDeposits env state = any ((> 1) . length) . group . sort . mapMaybe (normalizeDeposit . getAction)
  where
    normalizeDeposit :: Action -> Maybe Action
    normalizeDeposit = \case
      Deposit account party token value ->
        Just $ Deposit account party token $ Constant $ evalValue env state value
      _ -> Nothing

isTrueNotify :: Environment -> State -> Action -> Bool
isTrueNotify env state = \case
  Notify obs -> not $ evalObservation env state obs
  _ -> False
