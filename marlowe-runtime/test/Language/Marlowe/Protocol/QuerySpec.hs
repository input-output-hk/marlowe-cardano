{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.QuerySpec where

import Data.Foldable (fold)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), MarloweVersionTag (V1))
import Language.Marlowe.Runtime.Discovery.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Query.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweQuery protocol" do
  prop "Has a lawful codec" $ checkPropCodec @MarloweQuery
  codecGoldenTests @MarloweQuery "MarloweQuery"

instance ArbitraryRequest MarloweSyncRequest where
  arbitraryTag =
    elements
      [ SomeTag TagContractHeaders
      , SomeTag TagContractState
      , SomeTag TagTransaction
      , SomeTag TagTransactions
      , SomeTag TagWithdrawal
      , SomeTag TagWithdrawals
      ]
  arbitraryReq = \case
    TagContractHeaders -> ReqContractHeaders <$> arbitrary <*> arbitrary
    TagContractState -> ReqContractState <$> arbitrary
    TagTransaction -> ReqTransaction <$> arbitrary
    TagTransactions -> ReqTransactions <$> arbitrary
    TagWithdrawal -> ReqWithdrawal <$> arbitrary
    TagWithdrawals -> ReqWithdrawals <$> arbitrary <*> arbitrary
    TagStatus -> pure ReqStatus

  shrinkReq = \case
    ReqContractHeaders cFilter range ->
      fold
        [ ReqContractHeaders <$> shrink cFilter <*> pure range
        , ReqContractHeaders cFilter <$> shrink range
        ]
    ReqContractState contractId -> ReqContractState <$> shrink contractId
    ReqTransaction txId -> ReqTransaction <$> shrink txId
    ReqTransactions contractId -> ReqTransactions <$> shrink contractId
    ReqWithdrawal txId -> ReqWithdrawal <$> shrink txId
    ReqWithdrawals wFilter range ->
      fold
        [ ReqWithdrawals <$> shrink wFilter <*> pure range
        , ReqWithdrawals wFilter <$> shrink range
        ]
    ReqStatus -> []

  arbitraryResult = \case
    TagContractHeaders -> arbitrary
    TagContractState -> arbitrary
    TagTransaction -> arbitrary
    TagTransactions -> arbitrary
    TagWithdrawal -> arbitrary
    TagWithdrawals -> arbitrary
    TagStatus -> arbitrary

  shrinkResult = \case
    TagContractHeaders -> shrink
    TagContractState -> shrink
    TagTransaction -> shrink
    TagTransactions -> shrink
    TagWithdrawal -> shrink
    TagWithdrawals -> shrink
    TagStatus -> shrink

instance Arbitrary SomeContractState where
  arbitrary = SomeContractState MarloweV1 <$> arbitrary
  shrink (SomeContractState MarloweV1 state) = SomeContractState MarloweV1 <$> shrink state

instance Arbitrary RuntimeStatus where
  arbitrary =
    RuntimeStatus
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink RuntimeStatus{..} =
    fold
      [ [RuntimeStatus{nodeTip = x, ..} | x <- shrink nodeTip]
      , [RuntimeStatus{nodeTipUTC = x, ..} | x <- shrink nodeTipUTC]
      , [RuntimeStatus{runtimeChainTip = x, ..} | x <- shrink runtimeChainTip]
      , [RuntimeStatus{runtimeChainTipUTC = x, ..} | x <- shrink runtimeChainTipUTC]
      , [RuntimeStatus{runtimeTip = x, ..} | x <- shrink runtimeTip]
      , [RuntimeStatus{runtimeTipUTC = x, ..} | x <- shrink runtimeTipUTC]
      , [RuntimeStatus{networkId = x, ..} | x <- shrink networkId]
      , [RuntimeStatus{runtimeVersion = x, ..} | x <- shrink runtimeVersion]
      ]

instance Arbitrary SomeTransaction where
  arbitrary = SomeTransaction MarloweV1 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (SomeTransaction MarloweV1 input consumedBy state) =
    fold
      [ SomeTransaction MarloweV1 input consumedBy <$> shrink state
      , SomeTransaction MarloweV1 input <$> shrink consumedBy <*> pure state
      ]

instance Arbitrary SomeTransactions where
  arbitrary = SomeTransactions MarloweV1 <$> arbitrary
  shrink (SomeTransactions MarloweV1 txs) = SomeTransactions MarloweV1 <$> shrink txs

instance Arbitrary (ContractState 'V1) where
  arbitrary =
    ContractState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance (Arbitrary a, Arbitrary b) => Arbitrary (Page a b) where
  arbitrary = Page <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Range a) where
  arbitrary = Range <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary PayoutRef where
  arbitrary = PayoutRef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Withdrawal where
  arbitrary = Withdrawal <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Order where
  arbitrary = elements [Ascending, Descending]

instance Arbitrary ContractFilter where
  arbitrary = ContractFilter <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary WithdrawalFilter where
  arbitrary = WithdrawalFilter <$> arbitrary
  shrink = genericShrink
