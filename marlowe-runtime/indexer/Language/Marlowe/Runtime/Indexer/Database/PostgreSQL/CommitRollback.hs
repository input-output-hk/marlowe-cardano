{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback
  where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api

commitRollback :: ChainPoint -> H.Transaction ()
commitRollback point = H.statement (prepareParams point)
  [resultlessStatement|
    WITH blockUpdates (blockId) AS
      (    UPDATE marlowe.block
              SET rollbackToSlot = $1 :: bigint
                , rollbackToBlock = $2 :: bytea
            WHERE slotNo > $1 :: bigint
        RETURNING id
      )
    , deleteWithdrawalTxIns AS
      ( DELETE FROM marlowe.withdrawalTxIn as withdrawalTxIn
        USING blockUpdates
        WHERE withdrawalTxIn.blockId = blockUpdates.blockId
      )
    , deleteApplyTxs AS
      ( DELETE FROM marlowe.applyTx as applyTx
        USING blockUpdates
        WHERE applyTx.blockId = blockUpdates.blockId
      )
    , deleteCreateTxOuts AS
      ( DELETE FROM marlowe.createTxOut as createTxOut
        USING blockUpdates
        WHERE createTxOut.blockId = blockUpdates.blockId
      )
    , deletePayoutTxOuts AS
      ( DELETE FROM marlowe.payoutTxOut as payoutTxOut
        USING blockUpdates
        WHERE payoutTxOut.blockId = blockUpdates.blockId
      )
    , deleteContractTxOuts AS
      ( DELETE FROM marlowe.contractTxOut as contractTxOut
        USING blockUpdates
        WHERE contractTxOut.blockId = blockUpdates.blockId
      )
    , deleteTxOuts AS
      ( DELETE FROM marlowe.txOut as txOut
        USING blockUpdates
        WHERE txOut.blockId = blockUpdates.blockId
      )
      DELETE FROM marlowe.txOutAsset as txOutAsset
      USING blockUpdates
      WHERE txOutAsset.blockId = blockUpdates.blockId
  |]

type QueryParams = (Int64, ByteString)

prepareParams :: ChainPoint -> QueryParams
prepareParams = \case
  Genesis -> (-1, "")
  At BlockHeader{..} -> (fromIntegral slotNo, unBlockHeaderHash headerHash)
