-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Chain-sync client for Cardano node.
module Language.Marlowe.CLI.Sync (
  isMarloweOut,
  classifyOutputs,
) where

import Cardano.Api (
  AddressInEra (..),
  AddressTypeInEra (..),
  CtxTx,
  PaymentCredential (..),
  ScriptHash,
  SerialiseAsRawBytes (..),
  StakeAddressReference (..),
  TxId,
  TxIn (..),
  TxIx (..),
  TxOut (..),
  TxOutDatum (..),
  getScriptData,
  txOutValueToValue,
 )
import Cardano.Api.Byron (Address (..))
import Cardano.Api.Shelley (
  Address (..),
  StakeCredential (..),
  fromShelleyPaymentCredential,
  fromShelleyStakeReference,
  toPlutusData,
 )
import Cardano.Chain.Common (addrToBase58)
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA (length)
import Language.Marlowe.CLI.Sync.Types (
  MarloweOut (..),
 )
import PlutusLedgerApi.V1 (
  TokenName (..),
  fromData,
 )
import PlutusLedgerApi.V1 qualified as PV1

-- | Does a transaction output contain Marlowe information?
isMarloweOut
  :: MarloweOut
  -- ^ The transaction output.
  -> Bool
  -- ^ Whether there is Marlowe content.
isMarloweOut PlainOut{} = False
isMarloweOut _ = True

-- | Classify transaction outputs' Marlowe content.
classifyOutputs
  :: TxId
  -- ^ The transaction ID.
  -> [TxOut CtxTx era]
  -- ^ The transaction outputs.
  -> [MarloweOut]
  -- ^ The classified transaction outputs.
classifyOutputs txId txOuts =
  uncurry classifyOutput
    . first (TxIn txId . TxIx)
    <$> zip [0 ..] txOuts

-- | Classify a transaction output's Marlowe content.
classifyOutput
  :: TxIn
  -- ^ The transaction output ID.
  -> TxOut CtxTx era
  -- ^ The transaction output content.
  -> MarloweOut
  -- ^ The classified transaction output.
classifyOutput moTxIn (TxOut address value datum _) = case datum of
  TxOutDatumInTx _ (getScriptData -> datum') -> case (fromData $ toPlutusData datum', fromData $ toPlutusData datum') of
    (Just moOutput, _) -> ApplicationOut{..}
    (_, Just name) ->
      if BA.length name <= 32
        then let moPayout = TokenName name in PayoutOut{..}
        else PlainOut{..}
    _ -> PlainOut{..}
  _ -> PlainOut{..}
  where
    moAddress = toPlutusAddress address
    moValue = txOutValueToValue value

toPlutusAddress :: AddressInEra era -> PV1.Address
toPlutusAddress address = PV1.Address (cardanoAddressCredential address) (cardanoStakingCredential address)

cardanoAddressCredential :: AddressInEra era -> PV1.Credential
cardanoAddressCredential (AddressInEra ByronAddressInAnyEra (ByronAddress address)) =
  PV1.PubKeyCredential $
    PV1.PubKeyHash $
      PV1.toBuiltin $
        addrToBase58 address
cardanoAddressCredential (AddressInEra _ (ShelleyAddress _ paymentCredential _)) =
  case fromShelleyPaymentCredential paymentCredential of
    PaymentCredentialByKey paymentKeyHash ->
      PV1.PubKeyCredential $
        PV1.PubKeyHash $
          PV1.toBuiltin $
            serialiseToRawBytes paymentKeyHash
    PaymentCredentialByScript scriptHash ->
      PV1.ScriptCredential $ toPlutusScriptHash scriptHash

cardanoStakingCredential :: AddressInEra era -> Maybe PV1.StakingCredential
cardanoStakingCredential (AddressInEra ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (AddressInEra _ (ShelleyAddress _ _ stakeAddressReference)) =
  case fromShelleyStakeReference stakeAddressReference of
    NoStakeAddress -> Nothing
    (StakeAddressByValue stakeCredential) ->
      Just (PV1.StakingHash $ fromCardanoStakeCredential stakeCredential)
    StakeAddressByPointer{} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: StakeCredential -> PV1.Credential
    fromCardanoStakeCredential (StakeCredentialByKey stakeKeyHash) =
      PV1.PubKeyCredential $
        PV1.PubKeyHash $
          PV1.toBuiltin $
            serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (StakeCredentialByScript scriptHash) = PV1.ScriptCredential (toPlutusScriptHash scriptHash)

toPlutusScriptHash :: ScriptHash -> PV1.ScriptHash
toPlutusScriptHash = PV1.ScriptHash . PV1.toBuiltin . serialiseToRawBytes
