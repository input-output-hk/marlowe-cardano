{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Conway where

import Cardano.Ledger.Allegra.TxBody (StrictMaybe (..))
import Cardano.Ledger.Alonzo.Scripts (toAsIx)
import Cardano.Ledger.Alonzo.Tx (bodyAlonzoTxL, witsAlonzoTxL)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (TxDats (..), lookupRedeemer)
import Cardano.Ledger.Alonzo.UTxO (zipAsIxItem)
import Cardano.Ledger.Babbage (BabbageEra, BabbageTxOut)
import Cardano.Ledger.Babbage.Tx (
  IsValid (..),
 )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..), shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxWits,
  AsIxItem (..),
  Era (EraCrypto),
  EraTx,
  EraTxBody (inputsTxBodyL),
  PlutusPurpose,
  hashScript,
  hoistPlutusPurpose,
 )
import Cardano.Ledger.Conway.Scripts (
  AlonzoScript (..),
  ConwayPlutusPurpose (ConwaySpending),
 )
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxWits (AlonzoEraTxWits (rdmrsTxWitsL), AlonzoTxWits (..))
import Cardano.Ledger.Core (ScriptHash (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Data (dataToBinaryData)
import Cardano.Ledger.Shelley.API (TxIn)
import Control.Arrow (Arrow (..))
import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxOutRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (
  hashToBytea,
  originalBytea,
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
import Lens.Micro ((^.))
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
  :: forall era
   . (EraTx era)
  => (EraCrypto era ~ StandardCrypto)
  => (PlutusPurpose AsIxItem era ~ ConwayPlutusPurpose AsIxItem era)
  => (AlonzoEraTxWits era)
  => Int64
  -> Bytea
  -> Cardano.Ledger.Babbage.Tx.IsValid
  -> AlonzoTx era
  -> Set.Set (TxIn StandardCrypto)
  -> Set.Set (TxIn StandardCrypto)
  -> [TxInRow]
conwayTxInRows slot txId (Cardano.Ledger.Babbage.Tx.IsValid isValid) tx inputs collateralInputs
  | isValid = conwayTxInRow slot txId tx <$> Set.toAscList inputs
  | otherwise = do
      TxInRow{..} <- shelleyTxInRow slot txId <$> Set.toAscList collateralInputs
      pure TxInRow{isCollateral = SqlBool True, ..}

conwayTxInRow
  :: forall era
   . (EraTx era)
  => (EraCrypto era ~ StandardCrypto)
  => (PlutusPurpose AsIxItem era ~ ConwayPlutusPurpose AsIxItem era)
  => (AlonzoEraTxWits era)
  => Int64
  -> Bytea
  -> AlonzoTx era
  -> TxIn StandardCrypto
  -> TxInRow
conwayTxInRow slotNo txInId tx txIn =
  (shelleyTxInRow slotNo txInId txIn)
    { redeemerDatumBytes = do
        let redeemers = tx ^. witsAlonzoTxL . rdmrsTxWitsL
            inputs = tx ^. bodyAlonzoTxL . inputsTxBodyL
        index <- listToMaybe $ join $ zipAsIxItem (Set.toList inputs) $ \asIxItem@(AsIxItem _ txIn') ->
          [asIxItem | txIn == txIn']
        let purpose = ConwaySpending @AsIxItem @era index
        (datum, _) <- lookupRedeemer (hoistPlutusPurpose toAsIx purpose) redeemers
        pure $ originalBytea $ dataToBinaryData datum
    }
