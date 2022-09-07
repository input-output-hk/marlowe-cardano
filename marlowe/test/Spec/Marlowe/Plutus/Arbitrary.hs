-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Generate random data for Plutus tests.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Plutus.Arbitrary
where


import Plutus.V1.Ledger.Api (Address (..), BuiltinData (..), Credential (..), Data (..), Datum (..), DatumHash (..),
                             Extended (..), Interval (..), LowerBound (..), ScriptContext (..), ScriptPurpose (..),
                             StakingCredential (..), TxId (..), TxInInfo (..), TxInfo (..), TxOut (..), TxOutRef (..),
                             UpperBound (..), ValidatorHash (..), Value (..))
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Spec.Marlowe.Semantics.Arbitrary
import Test.Tasty.QuickCheck (Arbitrary (..), frequency, suchThat)

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Plutus.V1.Ledger.Value as V (adaSymbol, adaToken, singleton)


instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary


instance Arbitrary BS.ByteString where
  arbitrary = BS8.pack <$> arbitrary


instance Arbitrary BuiltinByteString where
  arbitrary = stringToBuiltinByteString <$> arbitrary


instance Arbitrary BuiltinData where
  arbitrary = BuiltinData <$> arbitrary


instance Arbitrary Credential where
  arbitrary =
    do
      isPubKey <- frequency [(9, pure True), (1, pure False)]
      if isPubKey
        then PubKeyCredential <$> arbitrary
        else ScriptCredential <$> arbitrary


instance Arbitrary Data where
  arbitrary =
    frequency
      [
        ( 1, Constr <$> arbitrary <*> arbitrary `suchThat` ((< 5) . length))
      , ( 2, Map    <$> arbitrary `suchThat` ((< 5) . length)              )
      , ( 5, List   <$> arbitrary `suchThat` ((< 5) . length)              )
      , (10, I      <$> arbitrary              )
      , (20, B      <$> arbitrary              )
      ]


instance Arbitrary Datum where
  arbitrary = Datum <$> arbitrary


instance Arbitrary DatumHash where
  arbitrary = DatumHash <$> arbitrary


instance Arbitrary a => Arbitrary (Extended a) where
  arbitrary =
    frequency
      [
        (1, pure NegInf         )
      , (9, Finite <$> arbitrary)
      , (1, pure PosInf         )
      ]


instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = Interval <$> arbitrary <*> arbitrary


instance Arbitrary a => Arbitrary (LowerBound a) where
  arbitrary = LowerBound <$> arbitrary <*> arbitrary


instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> (Spending <$> arbitrary)


instance Arbitrary StakingCredential where
  arbitrary = StakingHash <$> arbitrary


instance Arbitrary TxId where
  arbitrary = TxId . stringToBuiltinByteString <$> arbitrary


instance Arbitrary TxInfo where
  arbitrary =
    do
      txInfoInputs <- arbitrary
      txInfoOutputs <- arbitrary
      txInfoFee <- V.singleton V.adaSymbol V.adaToken <$> arbitraryPositiveInteger
      txInfoValidRange <- arbitrary
      txInfoSignatories <- arbitrary
      txInfoData <- arbitrary
      let
        txInfoMint = mempty
        txInfoDCert = mempty
        txInfoWdrl = mempty
      txInfoId <- arbitrary
      pure TxInfo{..}


instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary


instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary


instance Arbitrary a => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary


instance Arbitrary ValidatorHash where
  arbitrary = ValidatorHash <$> arbitrary


instance Arbitrary Value where
  arbitrary = Value <$> arbitraryAssocMap arbitrary (arbitraryAssocMap arbitrary arbitrary)
