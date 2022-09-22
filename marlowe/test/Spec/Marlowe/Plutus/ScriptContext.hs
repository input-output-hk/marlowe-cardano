-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test the `Plutus.V1.Ledger.Contexts.ScriptContext` functions used by the Marlowe validators.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}


module Spec.Marlowe.Plutus.ScriptContext
  ( -- * Testing
    tests
  ) where


import Data.List (find)
import Data.Maybe (catMaybes)
import Plutus.V2.Ledger.Api
  ( Address(Address)
  , Credential(PubKeyCredential)
  , TxInInfo(txInInfoResolved)
  , TxInfo(TxInfo, txInfoData, txInfoInputs, txInfoOutputs, txInfoSignatories)
  , TxOut(TxOut, txOutAddress, txOutValue)
  )
import Plutus.V2.Ledger.Contexts (findDatum, findDatumHash, txSignedBy, valuePaidTo, valueSpent)
import Spec.Marlowe.Plutus.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Property, elements, forAll, property, suchThat, testProperty)

import qualified PlutusTx.AssocMap as AM (toList)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "ScriptContext"
    [
      testProperty "`findDatum`"     checkFindDatum
    , testProperty "`findDatumHash`" checkFindDatumHash
    , testProperty "`txSignedBy`"    checkSignedBy
    , testProperty "`valuePaidTo`"   checkValuePaid
    , testProperty "`valueSpent`"    checkValueSpent
    ]


-- | `findDatum` locates a datum in a script context.
checkFindDatum :: Property
checkFindDatum =
  property
    $ let
        gen =
          do
            txInfo@TxInfo{txInfoData} <- arbitrary
            isPresent <- arbitrary
            if isPresent && not (null $ AM.toList txInfoData)
              then (txInfo, ) <$> elements (fst <$> AM.toList txInfoData)
              else (txInfo, ) <$> arbitrary `suchThat` (flip notElem $ fst <$> AM.toList txInfoData)
      in
        forAll gen
          $ \(txInfo@TxInfo{txInfoData}, h) ->
            findDatum h txInfo == fmap snd (find ((== h) . fst) $ AM.toList txInfoData)


-- | `findDatumHash` locates a datum hash in a script context.
checkFindDatumHash :: Property
checkFindDatumHash =
  property
    $ let
        gen =
          do
            txInfo@TxInfo{txInfoData} <- arbitrary
            isPresent <- arbitrary
            if isPresent && not (null $ AM.toList txInfoData)
              then (txInfo, ) <$> elements (snd <$> AM.toList txInfoData)
              else (txInfo, ) <$> arbitrary `suchThat` (flip notElem $ snd <$> AM.toList txInfoData)
      in
        forAll gen
          $ \(txInfo@TxInfo{txInfoData}, d) ->
            findDatumHash d txInfo == fmap fst (find ((== d) . snd) $ AM.toList txInfoData)


-- | `txSignedBy` detects a signature in a script context.
checkSignedBy :: Property
checkSignedBy =
  property
    $ let
        gen =
          do
            txInfo@TxInfo{txInfoSignatories} <- arbitrary
            isPresent <- arbitrary
            if isPresent && not (null txInfoSignatories)
              then (txInfo, ) <$> elements txInfoSignatories
              else (txInfo, ) <$> arbitrary `suchThat` flip notElem txInfoSignatories
      in
        forAll gen
          $ \(txInfo@TxInfo{txInfoSignatories}, pkh) ->
            txSignedBy txInfo pkh == elem pkh txInfoSignatories


-- | `valuePaidTo` fetches correct total value paid in a script context.
checkValuePaid :: Property
checkValuePaid =
  property
    $ let
        gen =
          do
            txInfo@TxInfo{txInfoOutputs} <- arbitrary
            let
              getPkh (Address (PubKeyCredential pkh) _) = Just pkh
              getPkh _                                  = Nothing
              pkhs = catMaybes $ getPkh . txOutAddress <$> txInfoOutputs
            isPresent <- arbitrary
            if isPresent && not (null txInfoOutputs)
              then (txInfo, ) <$> elements pkhs
              else (txInfo, ) <$> arbitrary `suchThat` flip notElem pkhs
      in
        forAll gen
          $ \(txInfo@TxInfo{txInfoOutputs}, pkh) ->
            let
              matchPkh (TxOut (Address (PubKeyCredential pkh') _) _ _ _) = pkh == pkh'
              matchPkh _                                                 = False
            in
              valuePaidTo txInfo pkh == foldMap txOutValue (filter matchPkh txInfoOutputs)


-- | `valueSpent` fetches correct total value spent in a script context.
checkValueSpent :: Property
checkValueSpent =
  property
    . forAll arbitrary
    $ \txInfo@TxInfo{txInfoInputs} ->
      valueSpent txInfo == foldMap (txOutValue . txInInfoResolved) txInfoInputs
