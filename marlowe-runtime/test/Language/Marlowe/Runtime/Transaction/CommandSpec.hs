{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.CommandSpec
  where

import Cardano.Api (CardanoEra(..))
import Data.Foldable (fold)
import Data.Void (absurd)
import GHC.Show (showSpace)
import Gen.Cardano.Api.Typed (genTx, genTxBody)
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
  arbitraryTag = elements
    [ SomeTag $ TagCreate MarloweV1
    , SomeTag $ TagApplyInputs MarloweV1
    , SomeTag $ TagWithdraw MarloweV1
    , SomeTag TagSubmit
    ]

  arbitraryCmd = \case
    TagCreate MarloweV1 -> Create
      <$> arbitrary
      <*> pure MarloweV1
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    TagApplyInputs MarloweV1 -> ApplyInputs MarloweV1
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
    TagWithdraw MarloweV1 -> Withdraw MarloweV1
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
    TagWithdraw MarloweV1 -> hedgehog $ genTxBody BabbageEra
    TagSubmit -> arbitrary

  shrinkCommand = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> fold
      [ [ Create stake MarloweV1 wallet' roleTokenConfig metadata minAda contract | wallet' <- shrink wallet ]
      , [ Create stake MarloweV1 wallet roleTokenConfig' metadata minAda contract | roleTokenConfig' <- shrink roleTokenConfig ]
      , [ Create stake MarloweV1 wallet roleTokenConfig metadata' minAda contract | metadata' <- shrink metadata ]
      , [ Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract' | contract' <- shrink contract ]
      ]
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> fold
      [ [ ApplyInputs MarloweV1 wallet' contractId metadata invalidBefore invalidHereafter inputs | wallet' <- shrink wallet ]
      , [ ApplyInputs MarloweV1 wallet contractId metadata' invalidBefore invalidHereafter inputs | metadata' <- shrink metadata ]
      , [ ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs' | inputs' <- shrink inputs ]
      ]
    Withdraw MarloweV1 wallet contractId role ->
      [ Withdraw MarloweV1 wallet' contractId role | wallet' <- shrink wallet ]
    Submit{}-> []

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

instance CommandEq MarloweTxCommand where
  commandEq = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> \case
      Create stake' MarloweV1 wallet' roleTokenConfig' metadata' minAda' contract' ->
        stake == stake'
          && wallet == wallet'
          && roleTokenConfig == roleTokenConfig'
          && metadata == metadata'
          && minAda == minAda'
          && contract == contract'
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> \case
      ApplyInputs MarloweV1 wallet' contractId' metadata' invalidBefore' invalidHereafter' inputs' ->
        wallet == wallet'
          && contractId == contractId'
          && metadata == metadata'
          && invalidBefore == invalidBefore'
          && invalidHereafter == invalidHereafter'
          && inputs == inputs'
    Withdraw MarloweV1 wallet contractId role -> \case
      Withdraw MarloweV1 wallet' contractId' role' ->
        wallet == wallet'
          && contractId == contractId'
          && role == role'
    Submit tx -> \case
      Submit tx' -> tx == tx'

  jobIdEq = \case
    JobIdSubmit txId -> \case
      JobIdSubmit txId' -> txId == txId'

  statusEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  errEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  resultEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

instance ShowCommand MarloweTxCommand where
  showsPrecTag p = \case
    TagCreate MarloweV1 -> showParen (p >= 11)
      ( showString "TagCreate"
      . showSpace
      . showString "MarloweV1"
      )
    TagApplyInputs MarloweV1 -> showParen (p >= 11)
      ( showString "TagApplyInputs"
      . showSpace
      . showString "MarloweV1"
      )
    TagWithdraw MarloweV1 -> showParen (p >= 11)
      ( showString "TagWithdraw"
      . showSpace
      . showString "MarloweV1"
      )
    TagSubmit -> showString "TagSubmit"

  showsPrecCommand p = showParen (p >= 11) . \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract ->
      ( showString "Create"
      . showSpace
      . showsPrec 11 stake
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 roleTokenConfig
      . showSpace
      . showsPrec 11 metadata
      . showSpace
      . showsPrec 11 minAda
      . showSpace
      . showsPrec 11 contract
      )
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs ->
      ( showString "ApplyInputs"
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 metadata
      . showSpace
      . showsPrec 11 invalidBefore
      . showSpace
      . showsPrec 11 invalidHereafter
      . showSpace
      . showsPrec 11 inputs
      )
    Withdraw MarloweV1 wallet contractId role ->
      ( showString "Withdraw"
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 role
      )
    Submit tx ->
      ( showString "Submit"
      . showSpace
      . showsPrec 11 tx
      )

  showsPrecJobId p = \case
    JobIdSubmit txId -> showParen (p >= 11)
      ( showString "JobIdSubmit"
      . showSpace
      . showsPrec 11 txId
      )

  showsPrecStatus p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecErr p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecResult p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p
