-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- Enable the following options to dump the Plutus code for the validators.
--
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-plc #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

module Language.Marlowe.Scripts.OpenRole (
  mkOpenRoleValidator,
  openRoleValidator,
  openRoleValidatorBytes,
  openRoleValidatorHash,
) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types qualified as V1
import Language.Marlowe.Scripts qualified as V1.Scripts
import Ledger.Typed.Scripts (unsafeMkTypedValidator)
import Plutus.Script.Utils.Typed qualified as Script.Utils.Typed
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Script.Utils.Typed
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Scripts (mkValidatorScript)
import Plutus.V1.Ledger.Value (adaSymbol, getValue, valueOf)
import Plutus.V2.Ledger.Api (
  Redeemer (..),
  ScriptPurpose (Spending),
  SerializedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxOut (..),
  ValidatorHash,
  fromBuiltinData,
  getValidator,
 )
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified as Haskell

#ifdef TRACE_PLUTUS

import PlutusTx.Prelude (traceError, traceIfFalse)

#else

{-# INLINABLE traceError #-}
traceError :: BuiltinString -> a
traceError _ = error ()

{-# INLINABLE traceIfFalse #-}
traceIfFalse :: BuiltinString -> a -> a
traceIfFalse _ = id

#endif

-- By decoding only the part of the script context I was able
-- to bring down the size of the validator from 4928 to 4540 bytes.
data SubScriptContext = SubScriptContext
  { scriptContextTxInfo :: SubTxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq, Haskell.Show)

{-# INLINEABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. (PV2.UnsafeFromData a) => BuiltinString -> BuiltinData -> a
tracedUnsafeFrom label d = trace label $ PV2.unsafeFromBuiltinData d

instance Script.Utils.Typed.IsScriptContext SubScriptContext where
  {-# INLINEABLE mkUntypedValidator #-}
  mkUntypedValidator f d r p =
    check
      $ f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedStakeValidator #-}
  mkUntypedStakeValidator f r p =
    check
      $ f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

instance Eq SubScriptContext where
  {-# INLINEABLE (==) #-}
  SubScriptContext info purpose == SubScriptContext info' purpose' = info == info' && purpose == purpose'

data SubTxInfo = SubTxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: BuiltinData
  , txInfoOutputs :: BuiltinData
  , txInfoFee :: BuiltinData
  , txInfoMint :: BuiltinData
  , txInfoDCert :: BuiltinData
  , txInfoWdrl :: BuiltinData
  , txInfoValidRange :: BuiltinData
  , txInfoSignatories :: BuiltinData
  , txInfoRedeemers :: AssocMap.Map ScriptPurpose Redeemer
  , txInfoData :: BuiltinData
  , txInfoId :: BuiltinData
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

instance Eq SubTxInfo where
  {-# INLINEABLE (==) #-}
  SubTxInfo i ri o f m c w r s rs d tid == SubTxInfo i' ri' o' f' m' c' w' r' s' rs' d' tid' =
    i
      == i'
      && ri
      == ri'
      && o
      == o'
      && f
      == f'
      && m
      == m'
      && c
      == c'
      && w
      == w'
      && r
      == r'
      && s
      == s'
      && rs
      == rs'
      && d
      == d'
      && tid
      == tid'

{- Open Role validator - it releases role token(s) (you can put more coins of the same role token to it) based on a few conditions:

   1. Value should contain only min. ada and a specific role token(s).
   2. Transaction should spend Marlowe output which contains corresponding to thread token (the same currency symbol as
   role).
   3. The first Marlowe input in the redeemer to the Marlowe should be `IDeposit` with the same role party as the role token.

   We *won't* perform the following checks:
   1. Checking the thread token in the Marlowe account map would only possibly mislead the user - we are not able to fully
   validate if the thread token won't leak or didn't leak already ;-)

   2. Check if Marlowe uses the same currency symbol as thread token. This consistency check between currency
   symbol and thread token has to be performed off-chain by the coordination layer.

   3. We don't do any additional double spending checks. It seems that two open role validators for the same role
   are be allowed to run and we prevent for two different roles release by performing role vs Marlowe input check.

   Error codes:

   "1" - Marlowe input not found.
   "2" - Invalid own value - we expect only the role token(s) and min ADA.
   "3" - Invalid Marlowe redeemer.
   "4" - Missing thread token.
-}
mkOpenRoleValidator
  :: ValidatorHash
  -- ^ The hash of the corresponding Marlowe validator.
  -> ()
  -> ()
  -> SubScriptContext
  -- ^ The script context.
  -> Bool
mkOpenRoleValidator
  marloweValidatorHash
  _
  _
  SubScriptContext
    { scriptContextTxInfo = SubTxInfo{txInfoInputs, txInfoRedeemers}
    , scriptContextPurpose = Spending txOutRef
    } = do
    let threadTokenName :: V1.TokenName
        threadTokenName = V1.TokenName emptyByteString

        marloweValidatorAddress = scriptHashAddress marloweValidatorHash
        -- Single pass over the inputs to find the Marlowe input and the own input.
        -- \* We need both inputs to analyze the `Value` content (role token and complementary thread token).
        -- \* Additionally we need `marloweTxOutRef` to have access to the Marlowe redeemer.
        -- Currently Marlowe validator checks if there is only one Marlowe input so we don't have to.
        (ownInput, marloweInput) = do
          let go (Just o) (Just m) _ = (o, m)
              go _ _ [] = traceError "1" -- Marlowe input not found.
              go possibleOwn possibleMarlowe (txInput@TxInInfo{txInInfoOutRef, txInInfoResolved} : txInputs) = do
                -- Check if the input is own input.
                if txInInfoOutRef == txOutRef
                  then go (Just txInput) possibleMarlowe txInputs
                  else -- Check if the input is the Marlowe input.

                    if txOutAddress txInInfoResolved == marloweValidatorAddress
                      then go possibleOwn (Just txInput) txInputs
                      else go possibleOwn possibleMarlowe txInputs
          go Nothing Nothing txInfoInputs

        -- `evaluatingScriptCounting`

        -- Extract role token information from the own input `Value`.
        (currencySymbol, roleName) = do
          let valuesList = AssocMap.toList $ getValue $ txOutValue $ txInInfoResolved ownInput

          -- Should I use this `find (currencySymbol /= adaSymbol)`?

          -- Value should contain only min. ADA and a specific role token(s).
          case valuesList of
            [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
              | possibleAdaSymbol PlutusTxPrelude.== adaSymbol -> (currencySymbol, roleName)
            [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
            _ -> traceError "2" -- Invalid value - we expect only the role token(s).

        -- Check if the Marlowe redeemer is `IDeposit` with correct role name.
        marloweRedeemerOk = do
          let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
          case AssocMap.lookup (Spending marloweTxOutRef) txInfoRedeemers of
            Nothing -> traceError "3" -- Invalid Marlowe redeemer
            -- Let's decode lazely only the first input.
            Just (Redeemer bytes) -> case fromBuiltinData bytes of
              Just (firstInputBytes : _) -> case fromBuiltinData firstInputBytes of
                Just (V1.Scripts.MerkleizedTxInput (V1.IDeposit _ (V1.Role role) _ _) _) -> traceIfFalse "3" (role PlutusTxPrelude.== roleName)
                Just (V1.Scripts.Input (V1.IDeposit _ (V1.Role role) _ _)) -> traceIfFalse "3" (role PlutusTxPrelude.== roleName)
                _ -> traceError "3"
              _ -> traceError "3"

        -- Check the Marlowe input `Value` for the thread token.
        threadTokenOk = do
          let marloweValue = txOutValue $ txInInfoResolved marloweInput
          traceIfFalse "4" (valueOf marloweValue currencySymbol threadTokenName > 0)

    marloweRedeemerOk && threadTokenOk
mkOpenRoleValidator _ _ _ _ = False

-- We need typed validator. Usually this type is defined to associate
-- datum and redeemer types and then derive decoding. We don't use this
-- approach because it introduces unnecessary overhead.
data TypedOpenRoleValidator

-- Copied from marlowe-cardano. This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
openRoleValidator :: Script.Utils.Typed.TypedValidator TypedOpenRoleValidator
openRoleValidator =
  let mkUntypedMarloweValidator :: ValidatorHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      mkUntypedMarloweValidator mvh = Script.Utils.Typed.mkUntypedValidator (mkOpenRoleValidator mvh)

      untypedValidator :: Script.Utils.Typed.Validator
      untypedValidator =
        mkValidatorScript
          $ $$(PlutusTx.compile [||mkUntypedMarloweValidator||])
          `PlutusTx.applyCode` PlutusTx.liftCode V1.Scripts.marloweValidatorHash

      typedValidator :: Script.Utils.Typed.TypedValidator Script.Utils.Typed.Any
      typedValidator = unsafeMkTypedValidator (Script.Utils.Typed.Versioned untypedValidator Script.Utils.Typed.PlutusV2)
   in unsafeCoerce typedValidator

openRoleValidatorBytes :: SerializedScript
openRoleValidatorBytes = SBS.toShort . LBS.toStrict . serialise . getValidator $ Script.Utils.Typed.validatorScript openRoleValidator

openRoleValidatorHash :: ValidatorHash
openRoleValidatorHash = Script.Utils.Typed.validatorHash openRoleValidator

PlutusTx.makeLift ''SubTxInfo
PlutusTx.makeIsDataIndexed ''SubTxInfo [('SubTxInfo, 0)]

PlutusTx.makeLift ''SubScriptContext
PlutusTx.makeIsDataIndexed ''SubScriptContext [('SubScriptContext, 0)]
