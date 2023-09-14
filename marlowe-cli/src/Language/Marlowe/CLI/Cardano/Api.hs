{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.CLI.Cardano.Api (
  Value.txOutValue,
  Value.txOutValueValue,
  adjustMinimumUTxO,
  toMultiAssetSupportedInEra,
  toPlutusProtocolVersion,
  toTxOutDatumInTx,
  toTxOutDatumInline,
  toReferenceTxInsScriptsInlineDatumsSupportedInEra,
  fromReferenceTxInsScriptsInlineDatumsSupportedInEra,
  withShelleyBasedEra,
) where

import Cardano.Api (
  AddressInEra (..),
  IsShelleyBasedEra,
  Lovelace,
  MinimumUTxOError,
  MultiAssetSupportedInEra (MultiAssetInAlonzoEra, MultiAssetInBabbageEra),
  ScriptDataSupportedInEra (..),
  TxOut (..),
  TxOutValue (..),
  calculateMinimumUTxO,
  lovelaceToValue,
  selectLovelace,
  shelleyBasedEra,
 )
import Cardano.Api qualified as Api
import Cardano.Api qualified as C
import Cardano.Api.Shelley (
  ProtocolParameters,
  ReferenceScript,
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra),
 )
import Cardano.Api.Shelley qualified as CS
import Control.Monad.Except (liftEither)
import GHC.Natural (Natural, naturalToInteger)
import Language.Marlowe.CLI.Cardano.Api.Value qualified as Value
import Language.Marlowe.CLI.Orphans ()
import Plutus.V1.Ledger.Api (ProtocolVersion (ProtocolVersion))
import Plutus.V2.Ledger.Api qualified as PV2

withShelleyBasedEra :: forall era a. ScriptDataSupportedInEra era -> ((IsShelleyBasedEra era) => a) -> a
withShelleyBasedEra = \case
  ScriptDataInAlonzoEra -> id
  ScriptDataInBabbageEra -> id

toMultiAssetSupportedInEra :: ScriptDataSupportedInEra era -> MultiAssetSupportedInEra era
toMultiAssetSupportedInEra = \case
  ScriptDataInAlonzoEra -> MultiAssetInAlonzoEra
  ScriptDataInBabbageEra -> MultiAssetInBabbageEra

toReferenceTxInsScriptsInlineDatumsSupportedInEra
  :: ScriptDataSupportedInEra era -> Maybe (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)
toReferenceTxInsScriptsInlineDatumsSupportedInEra = \case
  ScriptDataInAlonzoEra -> Nothing
  ScriptDataInBabbageEra -> Just ReferenceTxInsScriptsInlineDatumsInBabbageEra

fromReferenceTxInsScriptsInlineDatumsSupportedInEra
  :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> ScriptDataSupportedInEra era
fromReferenceTxInsScriptsInlineDatumsSupportedInEra ReferenceTxInsScriptsInlineDatumsInBabbageEra = ScriptDataInBabbageEra

toPlutusProtocolVersion :: (Natural, Natural) -> ProtocolVersion
toPlutusProtocolVersion (major, minor) = ProtocolVersion (fromInteger . naturalToInteger $ major) (fromInteger . naturalToInteger $ minor)

-- | 2022-08 This function was written to compensate for a bug in Cardano's calculateMinimumUTxO. It's called by adjustMinimumUTxO below. We will eventually be able to remove it.
ensureAtLeastHalfAnAda :: C.Value -> C.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = selectLovelace origValue
    minLovelace = C.Lovelace 500_000

-- | Compute the `minAda` and adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO
  :: forall era
   . ScriptDataSupportedInEra era
  -> ProtocolParameters
  -- ^ The protocol parameters.
  -> AddressInEra era
  -- ^ The output address.
  -> C.TxOutDatum C.CtxTx era
  -- ^ The datum, if any.
  -> C.Value
  -- ^ The output value.
  -> ReferenceScript era
  -> Either MinimumUTxOError (Lovelace, Api.Value)
  -- ^ Action to compute the adjusted value.
adjustMinimumUTxO era protocol address datum origValue mRefScript =
  do
    let value = ensureAtLeastHalfAnAda origValue
        txOut =
          TxOut
            address
            (TxOutValue (toMultiAssetSupportedInEra era) value) -- (value <> calcDeficit))
            datum
            mRefScript
    minValue <- liftEither $ calculateMinimumUTxO (withShelleyBasedEra era shelleyBasedEra) txOut protocol
    let minLovelace = selectLovelace minValue
        deficit = minLovelace <> negate (minimum [selectLovelace value, minLovelace])
    pure (minLovelace, value <> lovelaceToValue deficit)

toTxOutDatumInTx :: ScriptDataSupportedInEra era -> PV2.Datum -> C.TxOutDatum C.CtxTx era
toTxOutDatumInTx era plutusDatum = do
  let scriptData = CS.fromPlutusData . PV2.toData $ plutusDatum
  C.TxOutDatumInTx era scriptData

toTxOutDatumInline :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> PV2.Datum -> C.TxOutDatum C.CtxTx era
toTxOutDatumInline era plutusDatum = do
  let scriptData = CS.fromPlutusData . PV2.toData $ plutusDatum
  C.TxOutDatumInline era scriptData
