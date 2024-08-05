{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.CLI.Cardano.Api (
  Value.txOutValue,
  Value.txOutValueValue,
  adjustMinimumUTxO,
  toPlutusMajorProtocolVersion,
  toTxOutDatumInTx,
  toTxOutDatumInline,
) where

import Cardano.Api (
  AddressInEra (..),
  BabbageEraOnwards (..),
  MaryEraOnwards (..),
  ShelleyBasedEra,
  TxOut (..),
  TxOutValue (..),
  babbageEraOnwardsToAlonzoEraOnwards,
  babbageEraOnwardsToShelleyBasedEra,
  calculateMinimumUTxO,
  lovelaceToValue,
  selectLovelace,
  toLedgerValue,
  unsafeHashableScriptData,
 )
import Cardano.Api qualified as Api
import Cardano.Api qualified as C
import Cardano.Api.Shelley (LedgerProtocolParameters (..), ReferenceScript)
import Cardano.Api.Shelley qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Coin qualified as C
import GHC.Natural (Natural, naturalToInteger)
import Language.Marlowe.CLI.Cardano.Api.Value qualified as Value
import Language.Marlowe.CLI.Orphans ()
import PlutusLedgerApi.Common (MajorProtocolVersion (..))
import PlutusLedgerApi.V2 qualified as PV2

toPlutusMajorProtocolVersion :: (Natural, Natural) -> MajorProtocolVersion
toPlutusMajorProtocolVersion = MajorProtocolVersion . fromInteger . naturalToInteger . fst

-- | 2022-08 This function was written to compensate for a bug in Cardano's calculateMinimumUTxO. It's called by adjustMinimumUTxO below. We will eventually be able to remove it.
ensureAtLeastHalfAnAda :: C.Value -> C.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = selectLovelace origValue
    minLovelace = C.Coin 500_000

-- | Compute the `minAda` and adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO
  :: forall era
   . BabbageEraOnwards era
  -> C.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> AddressInEra era
  -- ^ The output address.
  -> C.TxOutDatum C.CtxTx era
  -- ^ The datum, if any.
  -> C.Value
  -- ^ The output value.
  -> ReferenceScript era
  -> (C.Coin, Api.Value)
  -- ^ Action to compute the adjusted value.
adjustMinimumUTxO era protocol address datum origValue mRefScript = (minLovelace, value <> lovelaceToValue deficit)
  where
    value = ensureAtLeastHalfAnAda origValue
    txOutValue :: TxOutValue era
    txOutValue = case era of
      BabbageEraOnwardsBabbage -> TxOutValueShelleyBased shelleyEra $ toLedgerValue MaryEraOnwardsBabbage value
      BabbageEraOnwardsConway -> TxOutValueShelleyBased shelleyEra $ toLedgerValue MaryEraOnwardsConway value
    txOut = TxOut address txOutValue datum mRefScript
    minLovelace = calculateMinimumUTxO shelleyEra txOut $ unLedgerProtocolParameters protocol
    deficit = minLovelace <> negate (min (selectLovelace value) minLovelace)
    shelleyEra :: ShelleyBasedEra era
    shelleyEra = babbageEraOnwardsToShelleyBasedEra era

toTxOutDatumInTx :: BabbageEraOnwards era -> PV2.Datum -> C.TxOutDatum C.CtxTx era
toTxOutDatumInTx era plutusDatum = do
  let scriptData = CS.fromPlutusData . PV2.toData $ plutusDatum
  C.TxOutDatumInTx (babbageEraOnwardsToAlonzoEraOnwards era) $ unsafeHashableScriptData scriptData

toTxOutDatumInline :: BabbageEraOnwards era -> PV2.Datum -> C.TxOutDatum C.CtxTx era
toTxOutDatumInline era plutusDatum = do
  let scriptData = CS.fromPlutusData . PV2.toData $ plutusDatum
  C.TxOutDatumInline era $ unsafeHashableScriptData scriptData
