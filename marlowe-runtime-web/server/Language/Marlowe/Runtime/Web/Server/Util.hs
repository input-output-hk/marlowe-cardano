{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.Web.Server.Util
  where

import Data.Function (on)
import qualified Data.List as List

import Cardano.Api
  ( ScriptValidity(ScriptInvalid, ScriptValid)
  , TxScriptValidity(TxScriptValidity, TxScriptValidityNone)
  , makeSignedTransaction
  )
import Cardano.Api.Shelley (ShelleyLedgerEra, Tx(ShelleyTx), TxBody(ShelleyTxBody))
import qualified Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx (ValidatedTx(ValidatedTx))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxWitness (TxWitness(TxWitness))
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import qualified Cardano.Ledger.Core
import Cardano.Ledger.Era (Era(Crypto))
import Cardano.Ledger.Keys (KeyRole(Witness))
import Cardano.Ledger.Shelley.TxBody (WitVKey)
import Data.Set (Set)
import Servant.Pagination

applyRangeToAscList :: Eq f => (a -> f) -> Maybe f -> Int -> Int -> RangeOrder -> [a] -> Maybe [a]
applyRangeToAscList getField startFrom limit offset order =
  fmap (List.take limit . List.drop offset)
    . case startFrom of
        Nothing -> Just
        Just f -> \as ->
          let
            as' = dropWhile ((/= f) . getField) as
          in
            if null as' then Nothing else Just as'
    . case order of
        RangeDesc -> reverse
        RangeAsc -> id
    . List.nubBy (on (==) getField)

type WitVKeys era = Set (WitVKey 'Witness (Crypto (ShelleyLedgerEra era)))

makeSignedTxWithWitnessKeys ::
  ( ShelleyLedgerEra era ~ shelleyLedgerEra
  , Cardano.Ledger.Era.Era shelleyLedgerEra
  , Cardano.Ledger.Core.Tx shelleyLedgerEra ~ ValidatedTx shelleyLedgerEra
  , Cardano.Ledger.Core.Script shelleyLedgerEra ~ Cardano.Ledger.Alonzo.Scripts.Script shelleyLedgerEra
  ) => TxBody era
    -> WitVKeys era
    -> Maybe (Tx era)
makeSignedTxWithWitnessKeys txBody wtKeys = do
  let
    txScriptValidityToIsValid :: TxScriptValidity era -> Alonzo.IsValid
    txScriptValidityToIsValid TxScriptValidityNone = Alonzo.IsValid True
    txScriptValidityToIsValid (TxScriptValidity _ scriptValidity) = case scriptValidity of
      ScriptValid -> Alonzo.IsValid True
      ScriptInvalid -> Alonzo.IsValid False

  case (txBody, makeSignedTransaction [] txBody) of
    (ShelleyTxBody era txBody' _ _ txmetadata scriptValidity, ShelleyTx _ (ValidatedTx _ bkTxWitness _ _)) -> do
      let
        TxWitness _ bkBoot bkScripts bkDats bkRdmrs = bkTxWitness
        wt' =
          TxWitness
            wtKeys
            bkBoot
            bkScripts
            bkDats
            bkRdmrs

      Just $ ShelleyTx era $ ValidatedTx
        txBody'
        wt'
        (txScriptValidityToIsValid scriptValidity)
        (maybeToStrictMaybe txmetadata)
    _ -> Nothing

