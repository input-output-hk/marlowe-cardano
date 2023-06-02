{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Marlowe.Runtime.Core.Gen
  where

import Data.Foldable (Foldable(fold))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusCurrencySymbol)
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Instances ()

instance Arbitrary ContractId where
  arbitrary = ContractId <$> arbitrary

instance Arbitrary SomeMarloweVersion where
  arbitrary = pure $ SomeMarloweVersion MarloweV1

class
  ( Arbitrary (Contract v)
  , Arbitrary (Datum v)
  , Arbitrary (Inputs v)
  , Arbitrary (PayoutDatum v)
  , IsMarloweVersion v
  ) => ArbitraryMarloweVersion v

instance ArbitraryMarloweVersion 'V1

instance Arbitrary MarloweTransactionMetadata where
  arbitrary = patchMarloweTransactionMetadata <$> (MarloweTransactionMetadata <$> arbitrary <*> arbitrary)
  shrink = fmap patchMarloweTransactionMetadata . genericShrink

patchMarloweTransactionMetadata :: MarloweTransactionMetadata -> MarloweTransactionMetadata
patchMarloweTransactionMetadata = decodeMarloweTransactionMetadataLenient . encodeMarloweTransactionMetadata

instance Arbitrary MarloweMetadataTag where
  arbitrary = MarloweMetadataTag <$> arbitrary
  shrink = fmap MarloweMetadataTag . shrink . getMarloweMetadataTag

instance Arbitrary MarloweMetadata where
  arbitrary = MarloweMetadata <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (Transaction v) where
  arbitrary = Transaction
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary V1.MarloweParams where
  arbitrary = V1.MarloweParams . toPlutusCurrencySymbol <$> arbitrary

instance Arbitrary V1.MarloweData where
  arbitrary = V1.MarloweData <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (TransactionOutput v) where
  arbitrary = TransactionOutput <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance ArbitraryMarloweVersion v => Arbitrary (Payout v) where
  arbitrary = Payout <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (Payout address assets datum) = fold
    [ [ Payout address assets' datum | assets' <- shrink assets ]
    , [ Payout address assets datum' | datum' <- shrink datum ]
    ]

instance ArbitraryMarloweVersion v => Arbitrary (TransactionScriptOutput v) where
  arbitrary = TransactionScriptOutput <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (TransactionScriptOutput address assets utxo datum) = fold
    [ [ TransactionScriptOutput address assets' utxo datum | assets' <- shrink assets ]
    , [ TransactionScriptOutput address assets utxo datum' | datum' <- shrink datum ]
    ]
