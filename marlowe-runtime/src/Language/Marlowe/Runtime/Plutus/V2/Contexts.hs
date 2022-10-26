{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Language.Marlowe.Runtime.Plutus.V2.Contexts
  where

import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude

{-# INLINEABLE missingFromOutputValue #-}
missingFromOutputValue :: PV2.CurrencySymbol -> PV2.TxOut -> Bool
missingFromOutputValue currencySymbol PV2.TxOut{ txOutValue } =
  isNothing . AssocMap.lookup currencySymbol . PV2.getValue $ txOutValue

{-# INLINEABLE missingFromOutputsValue #-}
missingFromOutputsValue :: PV2.CurrencySymbol -> PV2.ScriptContext -> Bool
missingFromOutputsValue currencySymbol PV2.ScriptContext { scriptContextTxInfo} = do
  let
    PV2.TxInfo { txInfoOutputs } = scriptContextTxInfo
  all (missingFromOutputValue currencySymbol) txInfoOutputs

