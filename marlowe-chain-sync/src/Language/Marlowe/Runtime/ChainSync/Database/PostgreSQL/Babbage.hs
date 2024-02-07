{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage where

import Cardano.Binary (serialize')
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), TxDats, unTxDats)
import Cardano.Ledger.Babbage (AlonzoScript, BabbageEra, BabbageTxOut)
import Cardano.Ledger.Babbage.Core (EraScript (..))
import Cardano.Ledger.Babbage.Scripts (AlonzoScript (..))
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..), shelleyProtVer)
import qualified Cardano.Ledger.Binary as L
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, hashBinaryData)
import Cardano.Ledger.Shelley.API (ShelleyTxOut (..), StrictMaybe (..))
import Cardano.Ledger.Plutus.Data (binaryDataToData, hashBinaryData)
import Cardano.Ledger.Plutus.Language (Plutus (..))
import qualified Cardano.Ledger.Plutus.Language as P
import Cardano.Ledger.Shelley.API (ScriptHash (..), ShelleyTxOut (..), StrictMaybe (..))
import Control.Arrow (Arrow (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Int
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxInRows, alonzoTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryAssetMintRows, maryTxOutRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (hashToBytea, originalBytea)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

babbageTxToRows :: Int64 -> Bytea -> Bytea -> AlonzoTx (BabbageEra StandardCrypto) -> TxRowGroup
babbageTxToRows slotNo blockHash txId tx@AlonzoTx{..} =
  ( alonzoTxRow encodeBabbageMetadata slotNo blockHash txId (btbValidityInterval body) auxiliaryData isValid
  , alonzoTxInRows slotNo txId isValid tx (btbInputs body) (btbCollateral body)
  , babbageTxOutRows slotNo txId isValid (txdats' wits) (btbCollateralReturn body) $ toList $ btbOutputs body
  , maryAssetMintRows slotNo txId $ btbMint body
  , babbageTxScripts wits $ toList $ btbOutputs body
  )

encodeBabbageMetadata :: AlonzoTxAuxData (BabbageEra StandardCrypto) -> ByteString
encodeBabbageMetadata (AlonzoTxAuxData md _ _) = L.serialize' shelleyProtVer md

babbageTxScripts
  :: AlonzoTxWits (BabbageEra StandardCrypto)
  -> [Sized (BabbageTxOut (BabbageEra StandardCrypto))]
  -> [ScriptRow]
babbageTxScripts AlonzoTxWits{..} outputs =
  uncurry babbageScriptRow <$> (Map.toList txscripts <> foldMap babbageReferenceScript outputs)

babbageReferenceScript
  :: Sized (BabbageTxOut (BabbageEra StandardCrypto))
  -> [(ScriptHash StandardCrypto, AlonzoScript (BabbageEra StandardCrypto))]
babbageReferenceScript (Sized (BabbageTxOut _ _ _ ref) _) = foldMap (pure . (hashScript &&& id)) ref

babbageScriptRow :: ScriptHash StandardCrypto -> AlonzoScript (BabbageEra StandardCrypto) -> ScriptRow
babbageScriptRow (ScriptHash hash) script =
  ScriptRow
    { scriptHash = hashToBytea hash
    , scriptBytes = originalBytea script
    , scriptLanguage = case script of
        TimelockScript _ -> Timelock
        PlutusScript (Plutus P.PlutusV1 _) -> PlutusV1
        PlutusScript _ -> PlutusV2
    }

babbageTxOutRows
  :: Int64
  -> Bytea
  -> IsValid
  -> TxDats (BabbageEra StandardCrypto)
  -> StrictMaybe (Sized (BabbageTxOut (BabbageEra StandardCrypto)))
  -> [Sized (BabbageTxOut (BabbageEra StandardCrypto))]
  -> [TxOutRowGroup]
babbageTxOutRows slot txId' (IsValid isValid) dats collateralReturn outputs
  | isValid = zipWith (babbageTxOutRow slot txId' dats) [0 ..] outputs
  | otherwise = case collateralReturn of
      SNothing -> []
      SJust (Sized (BabbageTxOut addr value _ _) _) ->
        case maryTxOutRow slot txId' (fromIntegral $ length outputs) $ ShelleyTxOut addr value of
          (TxOutRow{..}, assets) ->
            [(TxOutRow{isCollateral = SqlBool True, ..}, assets)]

babbageTxOutRow
  :: Int64
  -> Bytea
  -> TxDats (BabbageEra StandardCrypto)
  -> Int16
  -> Sized (BabbageTxOut (BabbageEra StandardCrypto))
  -> TxOutRowGroup
babbageTxOutRow slotNo txId dats txIx (Sized (BabbageTxOut addr value datum _) _) =
  case maryTxOutRow slotNo txId txIx (ShelleyTxOut addr value) of
    (txOut, assetOuts) -> (,assetOuts) case datum of
      NoDatum -> txOut
      DatumHash dh ->
        txOut
          { datumHash = Just $ originalBytea dh
          , datumBytes = fmap (Bytea . serialize') $ Map.lookup dh $ unTxDats dats
          }
      Datum d ->
        txOut
          { datumHash = Just $ originalBytea $ hashBinaryData d
          , datumBytes = Just $ Bytea $ serialize' $ binaryDataToData d
          }
