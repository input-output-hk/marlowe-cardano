{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Cardano.Api.Value where

import Cardano.Api qualified as C
import Language.Marlowe.CLI.Orphans ()
import Plutus.V1.Ledger.Ada qualified as PA
import PlutusLedgerApi.V1 qualified as P
import PlutusTx.Prelude qualified as PlutusTx

txOutValue :: C.TxOut ctx era -> C.TxOutValue era
txOutValue (C.TxOut _ v _ _) = v

txOutValueValue :: C.TxOut ctx era -> C.Value
txOutValueValue (C.TxOut _ v _ _) = C.txOutValueToValue v

toPlutusValue :: C.Value -> P.Value
toPlutusValue (C.valueToList -> assets) = foldMap (uncurry assetToValue) assets

lovelaceToPlutusAda :: C.Lovelace -> PA.Ada
lovelaceToPlutusAda (C.Lovelace v) = PA.Lovelace v

lovelaceToPlutusValue :: C.Lovelace -> P.Value
lovelaceToPlutusValue (C.Lovelace v) = P.singleton P.adaSymbol P.adaToken v

assetToValue :: C.AssetId -> C.Quantity -> P.Value
assetToValue (C.AssetId policyId assetName) (C.Quantity quantity) =
  P.singleton (toCurrencySymbol policyId) (toTokenName assetName) quantity
assetToValue C.AdaAssetId (C.Quantity quantity) =
  P.singleton P.adaSymbol P.adaToken quantity

toCurrencySymbol :: C.PolicyId -> P.CurrencySymbol
toCurrencySymbol = P.CurrencySymbol . P.toBuiltin . C.serialiseToRawBytes

toTokenName :: C.AssetName -> P.TokenName
toTokenName (C.AssetName bs) = P.TokenName $ PlutusTx.toBuiltin bs
