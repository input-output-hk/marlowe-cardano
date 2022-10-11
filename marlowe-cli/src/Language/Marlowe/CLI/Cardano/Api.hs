{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.CLI.Cardano.Api
  ( Value.txOutValue
  , Value.txOutValueValue
  , adjustMinimumUTxO
  , toMultiAssetSupportedInEra
  , toPlutusProtocolVersion
  , toReferenceTxInsScriptsInlineDatumsSupportedInEra
  , withShelleyBasedEra
  ) where


import Cardano.Api
  ( AddressInEra(..)
  , IsShelleyBasedEra
  , Lovelace
  , MinimumUTxOError
  , MultiAssetSupportedInEra(MultiAssetInAlonzoEra, MultiAssetInBabbageEra)
  , ScriptDataSupportedInEra(..)
  , TxOut(..)
  , TxOutDatum(..)
  , TxOutValue(..)
  , calculateMinimumUTxO
  , lovelaceToValue
  , selectLovelace
  , shelleyBasedEra
  )
import qualified Cardano.Api as Api (Value)
import qualified Cardano.Api as C
import Cardano.Api.Shelley
  ( ProtocolParameters
  , ReferenceScript
  , ReferenceTxInsScriptsInlineDatumsSupportedInEra(ReferenceTxInsScriptsInlineDatumsInBabbageEra)
  , fromPlutusData
  )
import Control.Monad.Except (liftEither)
import GHC.Natural (Natural, naturalToInteger)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as Value
import Language.Marlowe.CLI.Orphans ()
import Plutus.V1.Ledger.Api (ProtocolVersion(ProtocolVersion))
import Plutus.V2.Ledger.Api (Datum(..), toData)


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


toPlutusProtocolVersion :: (Natural, Natural) -> ProtocolVersion
toPlutusProtocolVersion (major, minor) = ProtocolVersion (fromInteger . naturalToInteger $ major) (fromInteger . naturalToInteger $ minor)


-- | 2022-08 This function was written to compensate for a bug in Cardano's calculateMinimumUTxO. It's called by adjustMinimumUTxO below. We will eventually be able to remove it.
ensureAtLeastHalfAnAda :: Api.Value -> Api.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = selectLovelace origValue
    minLovelace = C.Lovelace 500_000


-- | Compute the `minAda` and adjust the lovelace in an output to confirm to the minimum ADA requirement.
adjustMinimumUTxO :: forall era
                  . ScriptDataSupportedInEra era
                  -> ProtocolParameters       -- ^ The protocol parameters.
                  -> AddressInEra era         -- ^ The output address.
                  -> Maybe Datum              -- ^ The datum, if any.
                  -> Api.Value                -- ^ The output value.
                  -> ReferenceScript era
                  -> Either MinimumUTxOError (Lovelace, Api.Value)  -- ^ Action to compute the adjusted value.
adjustMinimumUTxO era protocol address datum origValue mRefScript =
  do
    let
      value = ensureAtLeastHalfAnAda origValue
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

