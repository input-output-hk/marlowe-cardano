{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO
  where

import Control.Arrow ((&&&))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Int (Int16, Int64)
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Indexer.Types

getMarloweUTxO :: BlockHeader -> H.Transaction MarloweUTxO
getMarloweUTxO BlockHeader{slotNo} = do
  unspentContractOutputs <- Map.fromDistinctAscList . V.toList . fmap decodeContractOutputRow <$> H.statement (fromIntegral slotNo) [vectorStatement|
    WITH contractOut (createTxId, createTxIx, txId, txIx, address, payoutScriptHash) AS
      ( SELECT createTxOut.txId
             , createTxOut.txIx
             , txOut.txId
             , txOut.txIx
             , txOut.address
             , contractTxOut.payoutScriptHash
          FROM marlowe.txOut
          JOIN marlowe.contractTxOut USING (txId, txIx)
          JOIN marlowe.createTxOut USING (txId, txIx)
          WHERE createTxOut.slotNo <= $1 :: bigint
         UNION
        SELECT applyTx.createTxId
             , applyTx.createTxIx
             , txOut.txId
             , txOut.txIx
             , txOut.address
             , contractTxOut.payoutScriptHash
          FROM marlowe.txOut
          JOIN marlowe.contractTxOut USING (txId, txIx)
          JOIN marlowe.applyTx ON txOut.txId = applyTx.txId AND txOut.txIx = applyTx.outputTxIx
          WHERE applyTx.slotNo <= $1 :: bigint
      )
    , contractIn (txId, txIx) AS
      ( SELECT applyTx.inputTxId
             , applyTx.inputTxIx
          FROM marlowe.applyTx
          WHERE applyTx.slotNo <= $1 :: bigint
         UNION
        SELECT invalidApplyTx.inputTxId
             , invalidApplyTx.inputTxIx
          FROM marlowe.invalidApplyTx
          JOIN marlowe.block ON block.id = invalidApplyTx.blockId
          WHERE block.slotNo <= $1 :: bigint
      )
    SELECT contractOut.createTxId :: bytea
         , contractOut.createTxIx :: smallint
         , contractOut.txId :: bytea
         , contractOut.txIx :: smallint
         , contractOut.address :: bytea
         , contractOut.payoutScriptHash :: bytea
      FROM contractOut
      LEFT JOIN contractIn USING (txId, txIx)
      WHERE contractIn.txId IS NULL
      ORDER BY contractOut.createTxId, contractOut.createTxIx
  |]


  unspentContractOutputsFlat <- V.toList . fmap decodePayoutOutputRow <$> H.statement (fromIntegral slotNo) [vectorStatement|
    WITH payoutOut (createTxId, createTxIx, txId, txIx) AS
      ( SELECT applyTx.createTxId
             , applyTx.createTxIx
             , txOut.txId
             , txOut.txIx
          FROM marlowe.txOut
          JOIN marlowe.payoutTxOut USING (txId, txIx)
          JOIN marlowe.applyTx USING (txId)
          WHERE applyTx.slotNo <= $1 :: bigint
      )
    , withdrawalIn (txId, txIx) AS
      ( SELECT withdrawalTxIn.payoutTxId
             , withdrawalTxIn.payoutTxIx
          FROM marlowe.withdrawalTxIn
          WHERE withdrawalTxIn.slotNo <= $1 :: bigint
      )
    SELECT payoutOut.createTxId :: bytea
         , payoutOut.createTxIx :: smallint
         , payoutOut.txId :: bytea
         , payoutOut.txIx :: smallint
      FROM payoutOut
      LEFT JOIN withdrawalIn USING (txId, txIx)
      WHERE withdrawalIn.txId IS NULL
      ORDER BY payoutOut.createTxId, payoutOut.createTxIx
  |]

  let
    unspentPayoutOutputs = Map.fromDistinctAscList
      $ fmap (fst . head &&& Set.fromList . fmap snd)
      $ groupBy (on (==) fst) unspentContractOutputsFlat

  pure MarloweUTxO{..}

decodeContractOutputRow
  :: (ByteString, Int16, ByteString, Int16, ByteString, ByteString)
  -> (Core.ContractId, UnspentContractOutput)
decodeContractOutputRow (createTxId, createTxIx, txId, txIx, address, payoutValidatorHash) =
  ( Core.ContractId $ TxOutRef (TxId createTxId) (fromIntegral createTxIx)
  , UnspentContractOutput
      { marloweVersion = Core.SomeMarloweVersion Core.MarloweV1
      , txOutRef = TxOutRef (TxId txId) (fromIntegral txIx)
      , marloweAddress = Address address
      , payoutValidatorHash = ScriptHash payoutValidatorHash
      }
  )

decodePayoutOutputRow
  :: (ByteString, Int16, ByteString, Int16)
  -> (Core.ContractId, TxOutRef)
decodePayoutOutputRow (createTxId, createTxIx, txId, txIx) =
  ( Core.ContractId $ TxOutRef (TxId createTxId) (fromIntegral createTxIx)
  , TxOutRef (TxId txId) (fromIntegral txIx)
  )

prepareBlockQueryParams :: BlockHeader -> (ByteString, Int64, Int64)
prepareBlockQueryParams BlockHeader{..} = (unBlockHeaderHash headerHash, fromIntegral slotNo, fromIntegral blockNo)
