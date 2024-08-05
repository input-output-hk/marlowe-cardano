{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo where

import Cardano.Binary (serialize')
import Cardano.Ledger.Allegra.Core (
  EraTx (Tx),
  EraTxAuxData (TxAuxData),
  ValidityInterval,
 )
import Cardano.Ledger.Alonzo (
  AlonzoEra,
  AlonzoScript,
  AlonzoTxAuxData,
  AlonzoTxOut,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..), txdats')
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (TxDats)
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import Cardano.Ledger.BaseTypes (shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (
  ScriptHash (..),
  ShelleyTxOut (ShelleyTxOut),
  StrictMaybe,
  TxIn,
 )
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int (Int16, Int64)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows, maryTxOutRow, maryTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (
  hashToBytea,
  mapStrictMaybe,
  originalBytea,
  serializeBytea,
  shelleyTxInRow,
 )
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types (
  Bytea (..),
  ScriptRow (..),
  SqlBool (..),
  TxInRow (..),
  TxOutRow (datumBytes, datumHash),
  TxOutRowGroup,
  TxRow,
  TxRowGroup,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types as Marlowe

alonzoTxToRows :: Int64 -> Bytea -> Bytea -> AlonzoTx (AlonzoEra StandardCrypto) -> TxRowGroup
alonzoTxToRows slotNo blockHash txId tx@AlonzoTx{..} =
  ( alonzoTxRow encodeAlonzoMetadata slotNo blockHash txId (atbValidityInterval body) auxiliaryData isValid
  , alonzoTxInRows slotNo txId isValid tx (atbInputs body) (atbCollateral body)
  , zipWith (alonzoTxOutRow slotNo txId $ txdats' wits) [0 ..] $ toList $ atbOutputs body
  , maryAssetMintRows slotNo txId $ atbMint body
  , alonzoTxScripts wits
  )

encodeAlonzoMetadata :: AlonzoTxAuxData (AlonzoEra StandardCrypto) -> ByteString
encodeAlonzoMetadata (AlonzoTxAuxData md _ _) = L.serialize' shelleyProtVer md

alonzoTxScripts :: Alonzo.AlonzoTxWits (AlonzoEra StandardCrypto) -> [ScriptRow]
alonzoTxScripts Alonzo.AlonzoTxWits{..} = uncurry alonzoScriptRow <$> Map.toList txscripts

alonzoScriptRow :: ScriptHash StandardCrypto -> AlonzoScript (AlonzoEra StandardCrypto) -> ScriptRow
alonzoScriptRow (ScriptHash hash) script =
  ScriptRow
    { scriptHash = hashToBytea hash
    , scriptBytes = serializeBytea shelleyProtVer script
    }

alonzoTxRow
  :: (TxAuxData era -> ByteString)
  -> Int64
  -> Bytea
  -> Bytea
  -> ValidityInterval
  -> StrictMaybe (TxAuxData era)
  -> IsValid
  -> TxRow
alonzoTxRow encodeMetadata slotNo blockHash txId validityInterval auxiliaryData isValid =
  (maryTxRow encodeMetadata slotNo blockHash txId validityInterval auxiliaryData)
    { Marlowe.isValid = convertIsValid isValid
    }

convertIsValid :: IsValid -> SqlBool
convertIsValid (IsValid b) = SqlBool b

alonzoTxInRows
  :: Int64
  -> Bytea
  -> IsValid
  -> Tx era
  -> Set.Set (TxIn StandardCrypto)
  -> Set.Set (TxIn StandardCrypto)
  -> [TxInRow]
alonzoTxInRows slot txId (IsValid isValid) tx inputs collateralInputs
  | isValid = alonzoTxInRow slot txId tx <$> Set.toAscList inputs
  | otherwise = do
      TxInRow{..} <- shelleyTxInRow slot txId <$> Set.toAscList collateralInputs
      pure TxInRow{isCollateral = SqlBool True, ..}

alonzoTxInRow
  :: Int64
  -> Bytea
  -> Tx era
  -> TxIn StandardCrypto
  -> TxInRow
alonzoTxInRow slotNo txInId _ = shelleyTxInRow slotNo txInId

alonzoTxOutRow
  :: Int64
  -> Bytea
  -> TxDats (AlonzoEra StandardCrypto)
  -> Int16
  -> AlonzoTxOut (AlonzoEra StandardCrypto)
  -> TxOutRowGroup
alonzoTxOutRow slotNo txId dats txIx (AlonzoTxOut addr value mDatumHash) =
  case maryTxOutRow slotNo txId txIx (ShelleyTxOut addr value) of
    (txOut, assetOuts) ->
      ( txOut
          { datumHash = mapStrictMaybe originalBytea mDatumHash
          , datumBytes =
              Bytea . serialize' <$> do
                datumHash <- mapStrictMaybe id mDatumHash
                Map.lookup datumHash $ Alonzo.unTxDats dats
          }
      , assetOuts
      )
