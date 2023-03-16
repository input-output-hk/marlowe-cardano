{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.History.Gen
  where

import Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Gen (ArbitraryMarloweVersion)
import Language.Marlowe.Runtime.History.Api
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Instances ()

instance Arbitrary ContractHistoryError where
  arbitrary = oneof
    [ pure HansdshakeFailed
    , FindTxFailed <$> arbitrary
    , ExtractContractFailed <$> arbitrary
    , FollowScriptUTxOFailed <$> arbitrary
    , FollowPayoutUTxOsFailed <$> arbitrary
    , ExtractMarloweTransactionFailed <$> arbitrary
    , PayoutUTxONotFound <$> arbitrary
    , pure CreateTxRolledBack
    ]
  shrink = genericShrink

instance Arbitrary ExtractCreationError where
  arbitrary = elements
    [ TxIxNotFound
    , ByronAddress
    , NonScriptAddress
    , InvalidScriptHash
    , NoCreateDatum
    , InvalidCreateDatum
    , NotCreationTransaction
    ]
  shrink = genericShrink

instance Arbitrary ExtractMarloweTransactionError where
  arbitrary = oneof
    [ pure TxInNotFound
    , pure NoRedeemer
    , pure InvalidRedeemer
    , pure NoTransactionDatum
    , pure InvalidTransactionDatum
    , NoPayoutDatum <$> arbitrary
    , InvalidPayoutDatum <$> arbitrary
    , pure InvalidValidityRange
    , pure SlotConversionFailed
    ]
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (CreateStep v) where
  arbitrary = CreateStep <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (RedeemStep v) where
  arbitrary = RedeemStep <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = const []

instance ArbitraryMarloweVersion v => Arbitrary (ContractStep v) where
  arbitrary = oneof
    [ ApplyTransaction <$> arbitrary
    , RedeemPayout <$> arbitrary
    ]
  shrink = const []
