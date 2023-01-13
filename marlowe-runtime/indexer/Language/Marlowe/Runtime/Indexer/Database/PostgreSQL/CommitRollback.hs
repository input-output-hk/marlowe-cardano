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
    , deleteApplyTxs (txId, txIx) AS
      ( DELETE FROM marlowe.applyTx as applyTx
        USING blockUpdates
        WHERE applyTx.blockId = blockUpdates.blockId
        RETURNING txId, outputTxIx
      )
    , deleteCreateTxOuts (txId, txIx) AS
      ( DELETE FROM marlowe.createTxOut as createTxOut
        USING blockUpdates
        WHERE createTxOut.blockId = blockUpdates.blockId
        RETURNING txId, txIx
      )
    , deletePayoutTxOuts (txId, txIx) AS
      ( DELETE FROM marlowe.payoutTxOut as payoutTxOut
        USING blockUpdates
        WHERE payoutTxOut.blockId = blockUpdates.blockId
        RETURNING txId, txIx
      )
    , deleteContractTxOuts (txId, txIx) AS
      ( DELETE FROM marlowe.contractTxOut as contractTxOut
        USING deleteApplyTxs, deleteCreateTxOuts
        WHERE ( contractTxOut.txId = deleteApplyTxs.txId
                AND contractTxOut.txIx = deleteApplyTxs.txIx
              ) OR
              ( contractTxOut.txId = deleteCreateTxOuts.txId
                AND contractTxOut.txIx = deleteCreateTxOuts.txIx
              )
        RETURNING txId, txIx
      )
    , deleteTxOuts (txId, txIx) AS
      ( DELETE FROM marlowe.txOuts as txOuts
        USING deleteContractTxOuts, deletePayoutTxOuts
        WHERE ( txOut.txId = deleteContractTxOuts.txId
                AND txOut.txIx = deleteContractTxOuts.txIx
              ) OR
              ( txOut.txId = deletePayoutTxOuts.txId
                AND txOut.txIx = deletePayoutTxOuts.txIx
              )
        RETURNING txId, txIx
      )
      DELETE FROM marlowe.txOutAsset as txOutAsset
      USING deleteTxOuts
      WHERE ( txOutAsset.txId = deleteTxOuts.txId
              AND txOutAsset.txIx = deleteTxOuts.txIx
            )
  |]

type QueryParams = (Int64, ByteString)

prepareParams :: ChainPoint -> QueryParams
prepareParams = \case
  Genesis -> (-1, "")
  At BlockHeader{..} -> (fromIntegral slotNo, unBlockHeaderHash headerHash)
