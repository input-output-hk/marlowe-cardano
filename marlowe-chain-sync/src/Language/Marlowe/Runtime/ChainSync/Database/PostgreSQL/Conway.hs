{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Conway where

import Cardano.Ledger.Allegra.TxBody (StrictMaybe (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))

import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (TxDats (..))
import Cardano.Ledger.Babbage (BabbageEra, BabbageTxOut)
import Cardano.Ledger.Babbage.Tx (
  IsValid (..),
 )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..), shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraTx (Tx), hashScript)
import Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
 )
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Core (ScriptHash (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API (TxIn)
import Control.Arrow (Arrow (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxOutRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (
  hashToBytea,
  serializeBytea,
  shelleyTxInRow,
 )
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types (
  Bytea,
  ScriptRow (..),
  SqlBool (SqlBool),
  TxInRow (..),
  TxRowGroup,
 )
import Unsafe.Coerce (unsafeCoerce)

conwayTxToRows :: Int64 -> Bytea -> Bytea -> AlonzoTx (ConwayEra StandardCrypto) -> TxRowGroup
conwayTxToRows slotNo blockHash txId tx@AlonzoTx{..} =
  ( alonzoTxRow encodeConwayMetadata slotNo blockHash txId (ctbVldt body) auxiliaryData isValid
  , conwayTxInRows slotNo txId isValid tx (ctbSpendInputs body) (ctbCollateralInputs body)
  , babbageTxOutRows
      slotNo
      txId
      isValid
      (coerceDats $ txdats' wits)
      (coerceTxOut <$> ctbCollateralReturn body)
      (coerceTxOut <$> toList (ctbOutputs body))
  , maryAssetMintRows slotNo txId $ ctbMint body
  , conwayTxScripts wits $ toList $ ctbOutputs body
  )

encodeConwayMetadata :: AlonzoTxAuxData (ConwayEra StandardCrypto) -> ByteString
encodeConwayMetadata (AlonzoTxAuxData md _ _) = L.serialize' shelleyProtVer md

conwayTxScripts
  :: AlonzoTxWits (ConwayEra StandardCrypto)
  -> [Sized (BabbageTxOut (ConwayEra StandardCrypto))]
  -> [ScriptRow]
conwayTxScripts AlonzoTxWits{..} outputs =
  uncurry conwayScriptRow <$> (Map.toList txscripts <> foldMap conwayReferenceScript outputs)

conwayReferenceScript
  :: Sized (BabbageTxOut (ConwayEra StandardCrypto))
  -> [(ScriptHash StandardCrypto, AlonzoScript (ConwayEra StandardCrypto))]
conwayReferenceScript (Sized (BabbageTxOut _ _ _ ref) _) = foldMap (pure . (hashScript &&& id)) ref

conwayScriptRow :: ScriptHash StandardCrypto -> AlonzoScript (ConwayEra StandardCrypto) -> ScriptRow
conwayScriptRow (ScriptHash hash) script =
  ScriptRow
    { scriptHash = hashToBytea hash
    , scriptBytes = serializeBytea shelleyProtVer script
    }

coerceTxOut
  :: Sized (BabbageTxOut (ConwayEra StandardCrypto))
  -> Sized (BabbageTxOut (BabbageEra StandardCrypto))
coerceTxOut (Sized (BabbageTxOut addr value datum _) size) =
  Sized (BabbageTxOut addr value (unsafeCoerce datum) SNothing) size

coerceDats :: TxDats (ConwayEra StandardCrypto) -> TxDats (BabbageEra StandardCrypto)
coerceDats = unsafeCoerce

conwayTxInRows
  :: Int64
  -> Bytea
  -> Cardano.Ledger.Babbage.Tx.IsValid
  -> Tx era
  -> Set.Set (TxIn StandardCrypto)
  -> Set.Set (TxIn StandardCrypto)
  -> [TxInRow]
conwayTxInRows slot txId (Cardano.Ledger.Babbage.Tx.IsValid isValid) tx inputs collateralInputs
  | isValid = conwayTxInRow slot txId tx <$> Set.toAscList inputs
  | otherwise = do
      TxInRow{..} <- shelleyTxInRow slot txId <$> Set.toAscList collateralInputs
      pure TxInRow{isCollateral = SqlBool True, ..}

conwayTxInRow
  :: Int64
  -> Bytea
  -> Tx era
  -> TxIn StandardCrypto
  -> TxInRow
conwayTxInRow slotNo txInId _ = shelleyTxInRow slotNo txInId
