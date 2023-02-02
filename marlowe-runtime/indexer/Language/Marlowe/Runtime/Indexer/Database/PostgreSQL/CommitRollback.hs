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
    WITH blockDeletes (blockId) AS
      ( DELETE FROM marlowe.block
        WHERE slotNo > $1 :: bigint
        RETURNING id
      )
    , blockInsert AS
      ( INSERT INTO marlowe.block (slotNo, id, blockNo)
        VALUES ($1 :: bigint, $2 :: bytea, $3 :: bigint)
        ON CONFLICT (id) DO NOTHING
      )
    , rollbackUpdates AS
      ( UPDATE marlowe.rollbackBlock
        SET
          toSlotNo = $1 :: bigint,
          toBlock = $2 :: bytea
        WHERE toSlotNo > $1 :: bigint
      )
    INSERT INTO marlowe.rollbackBlock (fromBlock, toSlotNo, toBlock)
    SELECT blockId, $1 :: bigint, $2 :: bytea
    FROM blockDeletes
  |]

type QueryParams = (Int64, ByteString, Int64)

prepareParams :: ChainPoint -> QueryParams
prepareParams = \case
  Genesis -> (-1, "", -1)
  At BlockHeader{..} -> (fromIntegral slotNo, unBlockHeaderHash headerHash, fromIntegral blockNo)
