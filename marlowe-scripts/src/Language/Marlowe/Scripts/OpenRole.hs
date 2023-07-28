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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- Enable the following options to dump the Plutus code for the validators.
--
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-plc #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

-- | Open Role validator
module Language.Marlowe.Scripts.OpenRole (
  mkOpenRoleValidator,
) where

import Language.Marlowe.Core.V1.Semantics.Types qualified as V1
import Language.Marlowe.Scripts qualified as V1.Scripts
import Plutus.V1.Ledger.Value (getValue, valueOf)
import Plutus.V2.Ledger.Api (
  Address,
  Redeemer (..),
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoInputs, txInfoRedeemers),
  fromBuiltinData,
 )
import Plutus.V2.Ledger.Tx (TxOut (txOutAddress, txOutValue))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude

{- Open Role validator - it releases role token(s) (you can put more coins of the same role token to it) based on a few conditions:

   1. Value should contain only min. ada and a specific role token(s).
   2. Transaction should spend Marlowe output which contains corresponding to thread token (the same currency symbol as
   role).
   3. The first Marlowe input in the redeemer to the Marlowe should be `IDeposit` with the same role party as the role token.

   We can consider optional checks which can be costly (decoding of the datum etc.) and don't necessarly add to the
   security of the schema:
   1. Check if Marlowe uses the same currency symbol as thread token. This can be pushed off-chain to the coordination
   layer.

   We *won't* perform the following checks:

   1. Checking the thread token in the Marlowe account map would only possibly mislead the user - we are not able to fully
   validate if the thread token won't leak or didn't leak already ;-)

   2. For double spending. Dobule spending in the context of this script has no sense - if we release the same role token
   twice to multiple `OpenRole` validators it is nearly an equivalent of releasing more of the role tokens into one of them.

   Error codes:

   "1" - Marlowe input not found.
   "2" - Invalid own value - we expect only the role token(s) and min ADA.
   "3" - Invalid Marlowe redeemer - we expect only the `IDeposit` redeemer.
   "4" - Missing thread token.
-}

threadTokenName = ""

mkOpenRoleValidator
  :: Address
  -> ()
  -> ()
  -> ScriptContext
  -- ^ The script context.
  -> Bool
mkOpenRoleValidator marloweValiadtorAddress _ _ ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs, txInfoRedeemers}, scriptContextPurpose = Spending txOutRef} = do
  let -- Single pass over the inputs to find the Marlowe input and the own input.
      -- \* We need both inputs to analyze the `Value` content (role token and complementary thread token).
      -- \* Additionally we need `marloweTxOutRef` to have access to the Marlowe redeemer.
      -- Currently Marlowe validator checks if there is only one Marlowe input so we don't have to.
      (ownInput, (marloweInput, marloweTxOutRef)) = do
        let go (Just o) (Just m) _ = (o, m)
            go _ _ [] = traceError "1" -- Marlowe input not found.
            go possibleOwn possibleMarlowe (txInput@TxInInfo{txInInfoOutRef, txInInfoResolved} : txInputs) = do
              -- Check if the input is own input.
              if txInInfoOutRef == txOutRef
                then go (Just txInput) possibleMarlowe txInputs
                else -- Check if the input is the Marlowe input.

                  if txOutAddress txInInfoResolved == marloweValiadtorAddress
                    then go possibleOwn (Just (txInput, txInInfoOutRef)) txInputs
                    else go possibleOwn possibleMarlowe txInputs
        go Nothing Nothing txInfoInputs

      -- Extract role token information from the own input `Value`.
      (currencySymbol, roleName) = do
        let valuesList = AssocMap.toList $ getValue $ txOutValue $ txInInfoResolved ownInput
        -- Value should contain only min. ADA and a specific role token(s).
        case valuesList of
          [("", _), (currencySymbol, AssocMap.toList -> [(roleName, _)])] -> (currencySymbol, roleName)
          [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
          _ -> traceError "2" -- Invalid value - we expect only the role token(s).

      -- Check if the Marlowe redeemer is `IDeposit` with correct role name.
      marloweRedeemerOk = case AssocMap.lookup (Spending marloweTxOutRef) txInfoRedeemers of
        Nothing -> traceError "3" -- Invalid Marlowe redeemer
        -- Let's decode lazely only the first input.
        Just (Redeemer bytes) -> case fromBuiltinData bytes of
          Just (firstInputBytes : _) -> case fromBuiltinData firstInputBytes of
            Just (V1.Scripts.MerkleizedTxInput (V1.IDeposit _ (V1.Role role) _ _) _) -> traceIfFalse "3" (role == roleName)
            Just (V1.Scripts.Input (V1.IDeposit _ (V1.Role role) _ _)) -> traceIfFalse "3" (role == roleName)
            _ -> traceError "3"
          _ -> traceError "3"

      -- We check the marlowe input Value to contain the thread token.
      threadTokenOk = do
        let marloweValue = txOutValue $ txInInfoResolved marloweInput
        traceIfFalse "4" (valueOf marloweValue currencySymbol threadTokenName > 0)
  marloweRedeemerOk && threadTokenOk
mkOpenRoleValidator _ _ _ _ = False
