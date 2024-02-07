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
  AlonzoTx (..),
  IsValid (..),
  indexRedeemers,
  txdats',
 )
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..), shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  AlonzoEraScript (PlutusPurpose),
  AlonzoEraTxWits,
  AsItem (AsItem),
  ConwayEraTxBody,
  Era (EraCrypto),
  EraTx (Tx),
 )
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (ConwaySpending))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Data (dataToBinaryData)
import Cardano.Ledger.Shelley.API (TxIn)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int (Int64)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxOutRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (originalBytea, shelleyTxInRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types (
  Bytea,
  SqlBool (SqlBool),
  TxInRow (..),
  TxRowGroup,
 )

import Cardano.Ledger.Conway.Scripts (AlonzoScript (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Core (EraScript (..), ScriptHash (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Language (Plutus (..))
import qualified Cardano.Ledger.Plutus.Language as P
import Control.Arrow (Arrow (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxInRows, alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxOutRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (hashToBytea, originalBytea)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types
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
    , scriptBytes = originalBytea script
    , scriptLanguage = case script of
        TimelockScript _ -> Timelock
        PlutusScript (Plutus P.PlutusV1 _) -> PlutusV1
        PlutusScript (Plutus P.PlutusV2 _) -> PlutusV2
        PlutusScript (Plutus P.PlutusV3 _) -> PlutusV3
    }

coerceTxOut
  :: Sized (BabbageTxOut (ConwayEra StandardCrypto))
  -> Sized (BabbageTxOut (BabbageEra StandardCrypto))
coerceTxOut (Sized (BabbageTxOut addr value datum _) size) =
  Sized (BabbageTxOut addr value (unsafeCoerce datum) SNothing) size

coerceDats :: TxDats (ConwayEra StandardCrypto) -> TxDats (BabbageEra StandardCrypto)
coerceDats = unsafeCoerce

conwayTxInRows
  :: ( ConwayEraTxBody era
     , AlonzoEraTxWits era
     , EraTx era
     , EraCrypto era ~ StandardCrypto
     , PlutusPurpose AsItem era
        ~ ConwayPlutusPurpose AsItem era
     )
  => Int64
  -> Bytea
  -> IsValid
  -> Tx era
  -> Set.Set (TxIn StandardCrypto)
  -> Set.Set (TxIn StandardCrypto)
  -> [TxInRow]
conwayTxInRows slot txId (IsValid isValid) tx inputs collateralInputs
  | isValid = conwayTxInRow slot txId tx <$> Set.toAscList inputs
  | otherwise = do
      TxInRow{..} <- shelleyTxInRow slot txId <$> Set.toAscList collateralInputs
      pure TxInRow{isCollateral = SqlBool True, ..}

conwayTxInRow
  :: ( ConwayEraTxBody era
     , EraTx era
     , EraCrypto era ~ StandardCrypto
     , PlutusPurpose AsItem era
        ~ ConwayPlutusPurpose AsItem era
     , AlonzoEraTxWits era
     )
  => Int64
  -> Bytea
  -> Tx era
  -> TxIn StandardCrypto
  -> TxInRow
conwayTxInRow slotNo txInId tx txIn =
  (shelleyTxInRow slotNo txInId txIn)
    { redeemerDatumBytes = do
        (datum, _) <- indexRedeemers tx $ ConwaySpending (AsItem txIn)
        pure $ originalBytea $ dataToBinaryData datum
    }
