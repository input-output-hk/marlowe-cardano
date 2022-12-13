{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}


module Language.Marlowe.Runtime.Plutus.V2.Scripts.MarloweV1.RoleTokensPolicy
  ( MintAction(..)
  , RoleTokens
  , mkRoleTokens
  , mkRoleTokensHash
  , policy
  ) where

import qualified PlutusTx
import PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude

import qualified Plutus.V2.Ledger.Api as PV2
import Plutus.V2.Ledger.Contexts as PV2
-- I was forced to extract types to a submodule
-- because of the `TempleteHaskell` expansion problem.
import Language.Marlowe.Runtime.Plutus.V2.Scripts.MarloweV1.RoleTokensPolicy.Types
  (MintAction(..), RoleTokens, RoleTokensHash, mkRoleTokens, mkRoleTokensHash)

-- | One shot policy which encodes the `txOutRef` and tokens minting in its hash.
-- In theory this currency can be reused accross multiple Marlowe Contracts when the precise
-- token distribution is not so important to the participant but rather a possible token
-- redundancy or uniquness.
mkPolicy ::
  RoleTokensHash ->
  PV2.TxOutRef ->
  MintAction ->
  PV2.ScriptContext ->
  Bool
mkPolicy roleTokensHash seedInput action context =
  case action of
    Mint -> validateMinting roleTokensHash seedInput context
    Burn -> validateBurning context

{-# INLINEABLE validateMinting #-}
validateMinting :: RoleTokensHash -> PV2.TxOutRef -> PV2.ScriptContext -> Bool
validateMinting roleTokens seedInput context =
  traceIfFalse "Mint failed" $
    seedInputIsConsumed
    && roleTokensAreMinted
  where
    PV2.ScriptContext{scriptContextTxInfo = txInfo} = context
    ownCurrency = PV2.ownCurrencySymbol context

    seedInputIsConsumed = do
      let PV2.TxOutRef txId txIx = seedInput
      PV2.spendsOutput txInfo txId txIx

    roleTokensAreMinted :: Bool
    roleTokensAreMinted = txMintedRoleTokens == roleTokens
      where
        txMintedRoleTokens = do
          let
            tokensMap = fromMaybe AssocMap.empty . AssocMap.lookup ownCurrency . PV2.getValue . PV2.txInfoMint $ txInfo
          mkRoleTokensHash . mkRoleTokens . AssocMap.toList $ tokensMap

{-# INLINEABLE validateBurning #-}
validateBurning :: PV2.ScriptContext -> Bool
validateBurning context = do
 traceIfFalse "Burn failed" allBurned
  where
    ownCurrency = PV2.ownCurrencySymbol context
    -- Allow only burning here
    allBurned = missingFromOutputsValue ownCurrency context

{-# INLINEABLE missingFromOutputsValue #-}
missingFromOutputsValue :: PV2.CurrencySymbol -> PV2.ScriptContext -> Bool
missingFromOutputsValue currencySymbol PV2.ScriptContext { scriptContextTxInfo} = do
  let
    PV2.TxInfo { txInfoOutputs } = scriptContextTxInfo
    missingFromOutputValue PV2.TxOut{ txOutValue } = isNothing . AssocMap.lookup currencySymbol . PV2.getValue $ txOutValue
  all missingFromOutputValue txInfoOutputs

policy :: RoleTokens -> PV2.TxOutRef -> PV2.MintingPolicy
policy roleTokens txOutRef = do
  let
    roleTokensHash = mkRoleTokensHash roleTokens
  PV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\rs seed -> wrapMintingPolicy (mkPolicy rs seed)||])
      `PlutusTx.applyCode` PlutusTx.liftCode roleTokensHash
      `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

-- Extracted from plutus-ledger

-- | Signature of an untyped minting policy script.
type MintingPolicyFn = BuiltinData -> BuiltinData -> ()

-- | Turns typed function into a minting policy which can be used
-- on the chain.
{-# INLINEABLE wrapMintingPolicy #-}
wrapMintingPolicy ::
  (PV2.UnsafeFromData redeemer, PV2.UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyFn
wrapMintingPolicy f r c =
  PlutusTx.Prelude.check (f redeemer context)
 where
  redeemer = PV2.unsafeFromBuiltinData r
  context = PV2.unsafeFromBuiltinData c
