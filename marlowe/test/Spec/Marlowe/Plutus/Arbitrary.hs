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
  (
  ) where


import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(..))
import Language.Marlowe.Scripts (MarloweTxInput(..))
import Plutus.V1.Ledger.Value (gt)
import Plutus.V2.Ledger.Api
  ( BuiltinData(..)
  , Data(..)
  , Datum(..)
  , DatumHash(..)
  , Extended(..)
  , Interval(..)
  , LowerBound(..)
  , OutputDatum(..)
  , Redeemer(..)
  , ScriptContext(..)
  , ScriptPurpose(..)
  , TxId(..)
  , TxInInfo(..)
  , TxInfo(..)
  , TxOut(..)
  , TxOutRef(..)
  , UpperBound(..)
  , Value(..)
  , adaSymbol
  , adaToken
  , singleton
  , toBuiltin
  )
import PlutusTx.Builtins (BuiltinByteString)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryAssocMap, arbitraryPositiveInteger)
import Spec.Marlowe.Semantics.Orphans ()
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, frequency, listOf, suchThat, vectorOf)

import qualified Data.ByteString as BS (ByteString, pack)
import qualified Data.ByteString.Char8 as BS8 (pack)


instance Arbitrary BS.ByteString where
  arbitrary = BS8.pack <$> arbitrary


instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> arbitrary


instance Arbitrary BuiltinData where
  arbitrary = BuiltinData <$> arbitrary


instance Arbitrary Data where
  arbitrary =
    let
      arbitraryDepth 0 =
        frequency
          [
            (1, I <$> arbitrary)
          , (2, B <$> arbitrary)
          ]
      arbitraryDepth n =
        frequency
          [
            ( 1, Constr <$> arbitrary <*> listOf (arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , ( 2, Map    <$> listOf ((,) <$> arbitraryDepth (n-1) <*> arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , ( 5, List   <$> listOf (arbitraryDepth (n-1)) `suchThat` ((< 5) . length))
          , (10, I      <$> arbitrary)
          , (20, B      <$> arbitrary)
          ]
    in
      arbitraryDepth (4 :: Int)


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


instance Arbitrary Redeemer where
  arbitrary = Redeemer <$> arbitrary


instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> (Spending <$> arbitrary)


instance Arbitrary ScriptPurpose where
  arbitrary =
    frequency
     [
       (2, Minting <$> arbitrary)
     , (8, Spending <$> arbitrary)
     ]


instance Arbitrary TxId where
  arbitrary = TxId <$> arbitraryByteString 32


instance Arbitrary TxInfo where
  arbitrary =
    do
      txInfoInputs <- arbitrary
      txInfoReferenceInputs <- arbitrary
      txInfoOutputs <- arbitrary
      txInfoFee <- singleton adaSymbol adaToken <$> arbitraryPositiveInteger
      txInfoValidRange <- arbitrary
      txInfoSignatories <- arbitrary
      txInfoRedeemers <- arbitraryAssocMap arbitrary arbitrary
      txInfoData <- arbitraryAssocMap arbitrary arbitrary
      let
        txInfoMint = mempty
        txInfoDCert = mempty
        txInfoWdrl = mempty
      txInfoId <- arbitrary
      pure TxInfo{..}


instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary


instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary `suchThat` (`gt` mempty) <*> (OutputDatumHash <$> arbitrary) <*> pure Nothing


instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary `suchThat` (> 0)


instance Arbitrary a => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary


instance Arbitrary Value where
  arbitrary = Value <$> arbitraryAssocMap arbitrary (arbitraryAssocMap arbitrary arbitrary)

instance Arbitrary MarloweParams where
  arbitrary = MarloweParams <$> arbitrary

instance Arbitrary MarloweData where
  arbitrary = MarloweData <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary MarloweTxInput where
  arbitrary =
    frequency
      [
        (19, Input             <$> arbitrary              )
      , ( 1, MerkleizedTxInput <$> arbitrary <*> arbitrary)
      ]


-- | Generate an arbitrary bytestring of specified length.
arbitraryByteString :: Int -> Gen BuiltinByteString
arbitraryByteString n = toBuiltin . BS.pack <$> vectorOf n arbitrary
