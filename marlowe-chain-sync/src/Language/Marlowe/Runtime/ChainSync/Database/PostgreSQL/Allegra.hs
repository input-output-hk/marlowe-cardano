{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Allegra where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..), ValidityInterval (..))
import Cardano.Ledger.BaseTypes (shelleyProtVer)
import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Core (TxAuxData)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (
  hashToBytea,
  mapStrictMaybe,
  serializeBytea,
  shelleyTxInRow,
  shelleyTxOutRow,
 )
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

allegraTxToRows :: Int64 -> Bytea -> Bytea -> ShelleyTx (AllegraEra StandardCrypto) -> TxRowGroup
allegraTxToRows slotNo blockHash txId ShelleyTx{..} =
  ( allegraTxRow encodeAllegraMetadata slotNo blockHash txId (atbValidityInterval body) auxiliaryData
  , shelleyTxInRow slotNo txId <$> Set.toAscList (atbInputs body)
  , zipWith (allegraTxOutRow slotNo txId) [0 ..] $ toList $ atbOutputs body
  , []
  , allegraTxScripts wits
  )

encodeAllegraMetadata :: AllegraTxAuxData (AllegraEra StandardCrypto) -> ByteString
encodeAllegraMetadata (AllegraTxAuxData md _) = serialize' shelleyProtVer md

allegraTxScripts :: ShelleyTxWits (AllegraEra StandardCrypto) -> [ScriptRow]
allegraTxScripts ShelleyTxWits{..} = uncurry allegraScriptRow <$> Map.toList scriptWits

allegraScriptRow :: ScriptHash StandardCrypto -> Timelock (AllegraEra StandardCrypto) -> ScriptRow
allegraScriptRow (ScriptHash hash) script =
  ScriptRow
    { scriptHash = hashToBytea hash
    , scriptBytes = serializeBytea shelleyProtVer script
    }

allegraTxRow
  :: (TxAuxData era -> ByteString)
  -> Int64
  -> Bytea
  -> Bytea
  -> ValidityInterval
  -> StrictMaybe (TxAuxData era)
  -> TxRow
allegraTxRow encodeMetadata slotNo blockHash txId validityInterval auxiliaryData =
  TxRow
    { blockHash
    , txId
    , slotNo
    , validityUpperBound
    , validityLowerBound
    , metadata =
        mapStrictMaybe (Bytea . encodeMetadata) auxiliaryData
    , isValid = SqlBool False
    }
  where
    ValidityIntervalFields{..} = validityIntervalFields validityInterval

validityIntervalFields :: ValidityInterval -> ValidityIntervalFields
validityIntervalFields ValidityInterval{..} =
  ValidityIntervalFields
    { validityLowerBound = mapStrictMaybe convertSlotNo invalidBefore
    , validityUpperBound = mapStrictMaybe convertSlotNo invalidHereafter
    }

data ValidityIntervalFields = ValidityIntervalFields
  { validityLowerBound :: Maybe Int64
  , validityUpperBound :: Maybe Int64
  }

allegraTxOutRow :: Int64 -> Bytea -> Int16 -> ShelleyTxOut (AllegraEra StandardCrypto) -> TxOutRowGroup
allegraTxOutRow slotNo txId txIx (ShelleyTxOut addr value) =
  shelleyTxOutRow slotNo txId txIx (ShelleyTxOut addr value)
