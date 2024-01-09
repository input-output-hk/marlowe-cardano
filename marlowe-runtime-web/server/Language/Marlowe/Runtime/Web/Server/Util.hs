{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.Web.Server.Util where

import Data.Function (on)
import qualified Data.List as List

import Cardano.Api (
  HasTextEnvelope (textEnvelopeType),
  HasTypeProxy (..),
  IsCardanoEra,
  IsShelleyBasedEra (..),
  ScriptValidity (..),
  SerialiseAsCBOR,
  ShelleyBasedEra (..),
  TxScriptValidity (..),
  makeSignedTransaction,
 )
import Cardano.Api.Byron (Tx (ByronTx), TxBody (..))
import Cardano.Api.Shelley (SerialiseAsCBOR (..), ShelleyLedgerEra, Tx (ShelleyTx), TxBody (ShelleyTxBody))
import Cardano.Ledger.Alonzo (AlonzoScript)
import Cardano.Ledger.Alonzo.Core (EraTxWits (..), Script)
import qualified Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), Decoder, decodeFullAnnotator, serialize')
import Cardano.Ledger.Binary.Decoding (Annotator)
import Cardano.Ledger.Core (eraProtVerLow)
import qualified Cardano.Ledger.Core
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Shelley.TxBody (WitVKey)
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Proxy (..))
import Data.Set (Set)
import Servant.Pagination

applyRangeToAscList :: (Eq f) => (a -> f) -> Maybe f -> Int -> Int -> RangeOrder -> [a] -> Maybe [a]
applyRangeToAscList getField startFrom limit offset order =
  fmap (List.take limit . List.drop offset)
    . case startFrom of
      Nothing -> Just
      Just f -> \as ->
        let as' = dropWhile ((/= f) . getField) as
         in if null as' then Nothing else Just as'
    . case order of
      RangeDesc -> reverse
      RangeAsc -> id
    . List.nubBy (on (==) getField)

type WitVKeys era = Set (WitVKey 'Witness (EraCrypto (ShelleyLedgerEra era)))

newtype TxWitnessSet era = TxWitnessSet (TxWits (ShelleyLedgerEra era))

instance (IsCardanoEra era) => HasTypeProxy (TxWitnessSet era) where
  data AsType (TxWitnessSet era) = AsTxWitnessSet (AsType era)
  proxyToAsType _ = AsTxWitnessSet $ proxyToAsType $ Proxy @era

instance
  ( IsCardanoEra era
  , EraTxWits (ShelleyLedgerEra era)
  , Script (ShelleyLedgerEra era) ~ AlonzoScript (ShelleyLedgerEra era)
  )
  => SerialiseAsCBOR (TxWitnessSet era)
  where
  serialiseToCBOR (TxWitnessSet wit) = serialize' (eraProtVerLow @(ShelleyLedgerEra era)) wit

  deserialiseFromCBOR _ bs = do
    let lbs = BSL.fromStrict bs

        annotator :: forall s. Decoder s (Annotator (TxWits (ShelleyLedgerEra era)))
        annotator = decCBOR

    (w :: TxWits (ShelleyLedgerEra era)) <-
      decodeFullAnnotator (eraProtVerLow @(ShelleyLedgerEra era)) "Tx Witness Set" annotator lbs
    pure $ TxWitnessSet w

instance
  ( IsShelleyBasedEra era
  , EraTxWits (ShelleyLedgerEra era)
  , Script (ShelleyLedgerEra era) ~ AlonzoScript (ShelleyLedgerEra era)
  )
  => HasTextEnvelope (TxWitnessSet era)
  where
  textEnvelopeType _ = do
    "TxWitness Set " <> case shelleyBasedEra @era of
      ShelleyBasedEraAlonzo -> "AlonzoEra"
      ShelleyBasedEraBabbage -> "BabbageEra"
      ShelleyBasedEraConway -> "ConwayEra"

makeSignedTxWithWitnessKeys
  :: forall era shelleyLedgerEra
   . ( IsShelleyBasedEra era
     , ShelleyLedgerEra era ~ shelleyLedgerEra
     , Cardano.Ledger.Era.Era shelleyLedgerEra
     , Cardano.Ledger.Core.TxWits shelleyLedgerEra ~ AlonzoTxWits shelleyLedgerEra
     , Cardano.Ledger.Core.Tx shelleyLedgerEra ~ AlonzoTx shelleyLedgerEra
     , Cardano.Ledger.Core.Script shelleyLedgerEra ~ Cardano.Ledger.Alonzo.Scripts.AlonzoScript shelleyLedgerEra
     )
  => TxBody era
  -> TxWitnessSet era
  -> Tx era
makeSignedTxWithWitnessKeys txBody (TxWitnessSet (AlonzoTxWits wtKeys _ _ _ _)) = do
  let txScriptValidityToIsValid :: TxScriptValidity era -> Alonzo.IsValid
      txScriptValidityToIsValid TxScriptValidityNone = Alonzo.IsValid True
      txScriptValidityToIsValid (TxScriptValidity _ scriptValidity) = case scriptValidity of
        ScriptValid -> Alonzo.IsValid True
        ScriptInvalid -> Alonzo.IsValid False

  case (txBody, makeSignedTransaction [] txBody) of
    (ShelleyTxBody era txBody' _ _ txMeta scriptValidity, ShelleyTx _ (AlonzoTx _ bkTxWitness _ _)) -> do
      let AlonzoTxWits _ bkBoot bkScripts bkDats bkRdmrs = bkTxWitness
          wt' =
            AlonzoTxWits @shelleyLedgerEra
              wtKeys
              bkBoot
              bkScripts
              bkDats
              bkRdmrs

      ShelleyTx era $
        AlonzoTx
          txBody'
          wt'
          (txScriptValidityToIsValid scriptValidity)
          (maybeToStrictMaybe txMeta)
    (ByronTxBody{}, _) -> case shelleyBasedEra @era of {}
    (_, ByronTx{}) -> case shelleyBasedEra @era of {}
