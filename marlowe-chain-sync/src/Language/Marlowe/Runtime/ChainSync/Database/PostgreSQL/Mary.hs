{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary where

import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData (..))
import Cardano.Ledger.Binary (serialize', shelleyProtVer)
import Cardano.Ledger.Core (TxAuxData)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..), ValidityInterval (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Shelley.API
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Foldable (Foldable (..))
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Allegra (allegraTxOutRow, allegraTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (hashToBytea, shelleyTxInRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

maryTxToRows :: Int64 -> Bytea -> Bytea -> ShelleyTx (MaryEra StandardCrypto) -> TxRowGroup
maryTxToRows slotNo blockHash txId ShelleyTx{..} =
  ( maryTxRow encodeMaryMetadata slotNo blockHash txId (mtbValidityInterval body) auxiliaryData
  , shelleyTxInRow slotNo txId <$> Set.toAscList (mtbInputs body)
  , zipWith (maryTxOutRow slotNo txId) [0 ..] $ toList $ mtbOutputs body
  , maryAssetMintRows slotNo txId $ mtbMint body
  )

encodeMaryMetadata :: AllegraTxAuxData (MaryEra StandardCrypto) -> ByteString
encodeMaryMetadata (AllegraTxAuxData md _) = serialize' shelleyProtVer md

maryTxRow
  :: (TxAuxData era -> ByteString)
  -> Int64
  -> Bytea
  -> Bytea
  -> ValidityInterval
  -> StrictMaybe (TxAuxData era)
  -> TxRow
maryTxRow encodeMetadata slotNo blockHash txId ValidityInterval{..} =
  allegraTxRow encodeMetadata slotNo blockHash txId Allegra.ValidityInterval{..}

maryTxOutRow :: Int64 -> Bytea -> Int16 -> ShelleyTxOut (MaryEra StandardCrypto) -> TxOutRowGroup
maryTxOutRow slotNo txId txIx (ShelleyTxOut addr (MaryValue lovelace assets)) =
  case allegraTxOutRow slotNo txId txIx (ShelleyTxOut addr lovelace) of
    (txOut, _) ->
      ( txOut
      , multiAssetRows (AssetOutRow txId txIx slotNo) assets
      )

maryAssetMintRows :: Int64 -> Bytea -> MultiAsset StandardCrypto -> [AssetMintRow]
maryAssetMintRows slotNo txId = multiAssetRows (AssetMintRow txId slotNo)

multiAssetRows :: (Bytea -> Bytea -> Int64 -> row) -> MultiAsset StandardCrypto -> [row]
multiAssetRows mkRow (MultiAsset assets) = do
  (PolicyID (ScriptHash policyId), tokens) <- Map.toAscList assets
  (AssetName token, quantity) <- Map.toAscList tokens
  pure $ mkRow (hashToBytea policyId) (Bytea $ fromShort token) (fromInteger quantity)
