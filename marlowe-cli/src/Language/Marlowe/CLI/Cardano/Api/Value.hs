{-# LANGUAGE ViewPatterns #-}
module Language.Marlowe.CLI.Cardano.Api.Value
  where

import qualified Cardano.Api as C
import Language.Marlowe.CLI.Orphans ()
import qualified Ledger.Tx.CardanoAPI as L
import qualified Plutus.V1.Ledger.Value as P
import qualified PlutusTx.Prelude as PlutusTx

txOutValue :: C.TxOut ctx era -> C.TxOutValue era
txOutValue (C.TxOut _ v _ _) = v


txOutValueValue :: C.TxOut ctx era -> C.Value
txOutValueValue (C.TxOut _ v _ _) = C.txOutValueToValue v


toPlutusValue :: C.Value -> P.Value
toPlutusValue (C.valueToList -> assets) = foldMap (uncurry assetToValue) assets


lovelaceToPlutusValue :: C.Lovelace -> P.Value
lovelaceToPlutusValue (C.Lovelace v) = P.singleton P.adaSymbol P.adaToken v


assetToValue :: C.AssetId -> C.Quantity -> P.Value
assetToValue (C.AssetId policyId assetName) (C.Quantity quantity) =
  P.singleton (toCurrencySymbol policyId) (toTokenName assetName) quantity
assetToValue C.AdaAssetId (C.Quantity quantity) =
  P.singleton P.adaSymbol P.adaToken quantity


toCurrencySymbol :: C.PolicyId -> P.CurrencySymbol
toCurrencySymbol = P.mpsSymbol . L.fromCardanoPolicyId


toTokenName :: C.AssetName -> P.TokenName
toTokenName (C.AssetName bs) = P.TokenName $ PlutusTx.toBuiltin bs


