{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Binary (serialize', shelleyProtVer)
import Cardano.Ledger.Core (TxAuxData)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Shelley.API (
  ScriptHash (..),
  ShelleyTx (..),
  ShelleyTxOut (ShelleyTxOut),
  ShelleyTxWits,
  StrictMaybe,
 )
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Foldable (Foldable (..))
import Data.Int (Int16, Int64)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Allegra (allegraTxOutRow, allegraTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (
  hashToBytea,
  serializeBytea,
  shelleyTxInRow,
 )
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types (
  AssetMintRow (AssetMintRow),
  AssetOutRow (AssetOutRow),
  Bytea (..),
  ScriptRow (..),
  TxOutRowGroup,
  TxRow,
  TxRowGroup,
 )

maryTxToRows :: Int64 -> Bytea -> Bytea -> Cardano.Ledger.Shelley.API.ShelleyTx (MaryEra StandardCrypto) -> TxRowGroup
maryTxToRows slotNo blockHash txId Cardano.Ledger.Shelley.API.ShelleyTx{..} =
  ( maryTxRow encodeMaryMetadata slotNo blockHash txId (mtbValidityInterval body) auxiliaryData
  , shelleyTxInRow slotNo txId <$> Set.toAscList (mtbInputs body)
  , zipWith (maryTxOutRow slotNo txId) [0 ..] $ toList $ mtbOutputs body
  , maryAssetMintRows slotNo txId $ mtbMint body
  , maryTxScripts wits
  )

encodeMaryMetadata :: AllegraTxAuxData (MaryEra StandardCrypto) -> ByteString
encodeMaryMetadata (AllegraTxAuxData md _) = serialize' shelleyProtVer md

maryTxScripts :: Cardano.Ledger.Shelley.API.ShelleyTxWits (MaryEra StandardCrypto) -> [ScriptRow]
maryTxScripts ShelleyTxWits{..} = uncurry maryScriptRow <$> Map.toList scriptWits

maryScriptRow :: Cardano.Ledger.Shelley.API.ScriptHash StandardCrypto -> Timelock (MaryEra StandardCrypto) -> ScriptRow
maryScriptRow (Cardano.Ledger.Shelley.API.ScriptHash hash) script =
  ScriptRow
    { scriptHash = hashToBytea hash
    , scriptBytes = Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley.serializeBytea shelleyProtVer script
    }

maryTxRow
  :: (TxAuxData era -> ByteString)
  -> Int64
  -> Bytea
  -> Bytea
  -> Allegra.ValidityInterval
  -> Cardano.Ledger.Shelley.API.StrictMaybe (TxAuxData era)
  -> TxRow
maryTxRow encodeMetadata slotNo blockHash txId Allegra.ValidityInterval{..} =
  allegraTxRow encodeMetadata slotNo blockHash txId Allegra.ValidityInterval{..}

maryTxOutRow
  :: Int64 -> Bytea -> Int16 -> Cardano.Ledger.Shelley.API.ShelleyTxOut (MaryEra StandardCrypto) -> TxOutRowGroup
maryTxOutRow slotNo txId txIx (Cardano.Ledger.Shelley.API.ShelleyTxOut addr (MaryValue lovelace assets)) =
  case allegraTxOutRow slotNo txId txIx (Cardano.Ledger.Shelley.API.ShelleyTxOut addr lovelace) of
    (txOut, _) ->
      ( txOut
      , multiAssetRows (AssetOutRow txId txIx slotNo) assets
      )

maryAssetMintRows :: Int64 -> Bytea -> MultiAsset StandardCrypto -> [AssetMintRow]
maryAssetMintRows slotNo txId = multiAssetRows (AssetMintRow txId slotNo)

multiAssetRows :: (Bytea -> Bytea -> Int64 -> row) -> MultiAsset StandardCrypto -> [row]
multiAssetRows mkRow (MultiAsset assets) = do
  (PolicyID (Cardano.Ledger.Shelley.API.ScriptHash policyId), tokens) <- Map.toAscList assets
  (AssetName token, quantity) <- Map.toAscList tokens
  pure $ mkRow (hashToBytea policyId) (Bytea $ fromShort token) (fromInteger quantity)
