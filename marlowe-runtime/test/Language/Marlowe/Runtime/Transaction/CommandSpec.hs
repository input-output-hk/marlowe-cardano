{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.CommandSpec where

import Cardano.Api (CardanoEra (..))
import Data.Foldable (fold)
import Data.Void (absurd)
import Gen.Cardano.Api.Typed (genTx)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Gen ()
import Network.Protocol.Codec.Spec (checkPropCodec)
import Network.Protocol.Job.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "MarloweTxCommand" do
  prop "It has a lawful Job protocol codec" $ checkPropCodec @(Job MarloweTxCommand)

instance ArbitraryCommand MarloweTxCommand where
  arbitraryTag =
    elements
      [ SomeTag $ TagCreate MarloweV1
      , SomeTag $ TagApplyInputs MarloweV1
      , SomeTag $ TagWithdraw MarloweV1
      , SomeTag TagSubmit
      ]

  arbitraryCmd = \case
    TagCreate MarloweV1 ->
      Create
        <$> arbitrary
        <*> pure MarloweV1
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    TagApplyInputs MarloweV1 ->
      ApplyInputs MarloweV1
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    TagWithdraw MarloweV1 ->
      Withdraw MarloweV1
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
    TagSubmit -> hedgehog $ Submit <$> genTx BabbageEra

  arbitraryJobId = \case
    TagCreate MarloweV1 -> Nothing
    TagApplyInputs MarloweV1 -> Nothing
    TagWithdraw MarloweV1 -> Nothing
    TagSubmit -> Just $ JobIdSubmit <$> arbitrary

  arbitraryStatus = \case
    TagCreate MarloweV1 -> Nothing
    TagApplyInputs MarloweV1 -> Nothing
    TagWithdraw MarloweV1 -> Nothing
    TagSubmit -> Just arbitrary

  arbitraryErr = \case
    TagCreate MarloweV1 -> Just arbitrary
    TagApplyInputs MarloweV1 -> Just arbitrary
    TagWithdraw MarloweV1 -> Just arbitrary
    TagSubmit -> Just arbitrary

  arbitraryResult = \case
    TagCreate MarloweV1 -> arbitrary
    TagApplyInputs MarloweV1 -> arbitrary
    TagWithdraw MarloweV1 -> arbitrary
    TagSubmit -> arbitrary

  shrinkCommand = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract ->
      fold
        [ [Create stake MarloweV1 wallet' roleTokenConfig metadata minAda contract | wallet' <- shrink wallet]
        , [Create stake MarloweV1 wallet roleTokenConfig' metadata minAda contract | roleTokenConfig' <- shrink roleTokenConfig]
        , [Create stake MarloweV1 wallet roleTokenConfig metadata' minAda contract | metadata' <- shrink metadata]
        , [Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract' | contract' <- shrink contract]
        ]
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs ->
      fold
        [ [ApplyInputs MarloweV1 wallet' contractId metadata invalidBefore invalidHereafter inputs | wallet' <- shrink wallet]
        , [ ApplyInputs MarloweV1 wallet contractId metadata' invalidBefore invalidHereafter inputs | metadata' <- shrink metadata
          ]
        , [ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs' | inputs' <- shrink inputs]
        ]
    Withdraw MarloweV1 wallet contractId role ->
      [Withdraw MarloweV1 wallet' contractId role | wallet' <- shrink wallet]
    Submit{} -> []

  shrinkJobId = \case
    JobIdSubmit{} -> []

  shrinkErr = \case
    TagCreate MarloweV1 -> shrink
    TagApplyInputs MarloweV1 -> shrink
    TagWithdraw MarloweV1 -> shrink
    TagSubmit -> shrink

  shrinkResult = \case
    TagCreate MarloweV1 -> shrink
    TagApplyInputs MarloweV1 -> shrink
    TagWithdraw MarloweV1 -> const []
    TagSubmit -> shrink

  shrinkStatus = \case
    TagCreate MarloweV1 -> absurd
    TagApplyInputs MarloweV1 -> absurd
    TagWithdraw MarloweV1 -> absurd
    TagSubmit -> shrink
