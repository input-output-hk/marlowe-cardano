{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO
  where

import Control.Arrow ((&&&))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Int (Int16, Int64)
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Hasql.TH (maybeStatement, vectorStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Indexer.Types

getMarloweUTxO :: BlockHeader -> H.Transaction (Maybe MarloweUTxO)
getMarloweUTxO block = runMaybeT do
  slotNo <- MaybeT $ H.statement (prepareBlockQueryParams block) [maybeStatement|
    SELECT slotNo :: bigint
      FROM marlowe.block
     WHERE id = $1 :: bytea AND slotNo = $2 :: bigint AND blockNo = $3 :: bigint AND rollbackToSlot IS NULL AND rollbackToBlock IS NULL
  |]


  unspentContractOutputs <- lift $ Map.fromDistinctAscList . V.toList . fmap decodeContractOutputRow <$> H.statement slotNo [vectorStatement|
    WITH block (blockId) AS
      ( SELECT id :: bytea
          FROM marlowe.block
         WHERE slotNo <= $1 :: bigint AND rollbackToSlot IS NULL AND rollbackToBlock IS NULL
      )
    , contractOut (createTxId, createTxIx, txId, txIx, address, payoutScriptHash) AS
      ( SELECT createTxOut.txId
             , createTxOut.txIx
             , txOut.txId
             , txOut.txIx
             , txOut.address
             , contractTxOut.payoutScriptHash
          FROM block
          JOIN marlowe.txOut AS txOut USING (blockId)
          JOIN marlowe.contractTxOut AS contractTxOut USING (txId, txIx)
          JOIN marlowe.createTxOut AS createTxOut USING (txId, txIx)
         UNION
        SELECT applyTx.createTxId
             , applyTx.createTxIx
             , txOut.txId
             , txOut.txIx
             , txOut.address
             , contractTxOut.payoutScriptHash
          FROM block
          JOIN marlowe.txOut AS txOut USING (blockId)
          JOIN marlowe.contractTxOut AS contractTxOut USING (txId, txIx)
          JOIN marlowe.applyTx AS applyTx ON txOut.txId = applyTx.txId AND txOut.txIx = applyTx.outputTxIx
      )
    , contractIn (txId, txIx) AS
      ( SELECT applyTx.inputTxId
             , applyTx.inputTxIx
          FROM block
          JOIN marlowe.applyTx AS applyTx USING (blockId)
         UNION
        SELECT invalidApplyTx.inputTxId
             , invalidApplyTx.inputTxIx
          FROM block
          JOIN marlowe.invalidApplyTx AS invalidApplyTx USING (blockId)
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


  unspentContractOutputsFlat <- lift $ V.toList . fmap decodePayoutOutputRow <$> H.statement slotNo [vectorStatement|
    WITH block (blockId) AS
      ( SELECT id :: bytea
          FROM marlowe.block
         WHERE slotNo <= $1 :: bigint AND rollbackToSlot IS NULL AND rollbackToBlock IS NULL
      )
    , payoutOut (createTxId, createTxIx, txId, txIx) AS
      ( SELECT applyTx.createTxId
             , applyTx.createTxIx
             , txOut.txId
             , txOut.txIx
          FROM block
          JOIN marlowe.txOut AS txOut USING (blockId)
          JOIN marlowe.payoutTxOut USING (txId, txIx)
          JOIN marlowe.applyTx AS applyTx USING (txId)
      )
    , withdrawalIn (txId, txIx) AS
      ( SELECT withdrawalTxIn.payoutTxId
             , withdrawalTxIn.payoutTxIx
          FROM block
          JOIN marlowe.withdrawalTxIn AS withdrawalTxIn USING (blockId)
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
