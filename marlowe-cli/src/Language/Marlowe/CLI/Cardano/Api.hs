{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}


module Language.Marlowe.CLI.Cardano.Api (
  adjustMinimumUTxO
, toMultiAssetSupportedInEra
, toReferenceTxInsScriptsInlineDatumsSupportedInEra
, withShelleyBasedEra
, toPlutusProtocolVersion
) where


import Cardano.Api (AddressInEra (..), IsShelleyBasedEra, Lovelace, MinimumUTxOError,
                    MultiAssetSupportedInEra (MultiAssetInAlonzoEra, MultiAssetInBabbageEra),
                    ScriptDataSupportedInEra (..), TxOut (..), TxOutDatum (..), TxOutValue (..), calculateMinimumUTxO,
                    lovelaceToValue, selectLovelace, shelleyBasedEra)
import qualified Cardano.Api as Api (Value)
import Cardano.Api.Shelley (ProtocolParameters, ReferenceScript,
                            ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra),
                            fromPlutusData)
import Control.Monad.Except (liftEither)
import GHC.Natural (Natural, naturalToInteger)
import Language.Marlowe.CLI.Orphans ()
import Plutus.V1.Ledger.Api (ProtocolVersion (ProtocolVersion))
import Plutus.V2.Ledger.Api (Datum (..), toData)


withShelleyBasedEra :: forall era a. ScriptDataSupportedInEra era -> (IsShelleyBasedEra era => a) -> a
withShelleyBasedEra = \case
  ScriptDataInAlonzoEra  -> id
  ScriptDataInBabbageEra -> id

toMultiAssetSupportedInEra :: ScriptDataSupportedInEra era -> MultiAssetSupportedInEra era
toMultiAssetSupportedInEra = \case
  ScriptDataInAlonzoEra  -> MultiAssetInAlonzoEra
  ScriptDataInBabbageEra -> MultiAssetInBabbageEra


toReferenceTxInsScriptsInlineDatumsSupportedInEra :: ScriptDataSupportedInEra era -> Maybe (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)
toReferenceTxInsScriptsInlineDatumsSupportedInEra = \case
  ScriptDataInAlonzoEra  -> Nothing
  ScriptDataInBabbageEra -> Just ReferenceTxInsScriptsInlineDatumsInBabbageEra


-- | Compute the `minAda` and adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO :: forall era
                  . ScriptDataSupportedInEra era
                  -> ProtocolParameters       -- ^ The protocol parameters.
                  -> AddressInEra era         -- ^ The output address.
                  -> Maybe Datum              -- ^ The datum, if any.
                  -> Api.Value                -- ^ The output value.
                  -> ReferenceScript era
                  -> Either MinimumUTxOError (Lovelace, Api.Value)  -- ^ Action to compute the adjusted value.
adjustMinimumUTxO era protocol address datum value mRefScript =
  do
    let
      -- minCalcLovelace = 500_000
      -- calcDeficit = maximum [selectLovelace value <> minCalcLovelace, 0]
      txOut =
        TxOut
          address
          (TxOutValue (toMultiAssetSupportedInEra era) value) -- (value <> calcDeficit))
          (maybe TxOutDatumNone (TxOutDatumInTx era . fromPlutusData . toData) datum)
          mRefScript
    minValue <- liftEither $ calculateMinimumUTxO (withShelleyBasedEra era shelleyBasedEra) txOut protocol
    let
      minLovelace = selectLovelace minValue
      deficit = minLovelace <> negate (minimum[selectLovelace value, minLovelace])
    pure (minLovelace, value <> lovelaceToValue deficit)


toPlutusProtocolVersion :: (Natural, Natural) -> ProtocolVersion
toPlutusProtocolVersion (major, minor) = ProtocolVersion (fromInteger . naturalToInteger $ major) (fromInteger . naturalToInteger $ minor)
