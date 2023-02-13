{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal
  where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (decodeBlockHeader, decodeTxOutRef)

getWithdrawal :: TxId -> T.Transaction (Maybe Withdrawal)
getWithdrawal txId = fmap decodeWithdrawal <$> T.statement (unTxId txId)
  [maybeStatement|
    SELECT
      txId :: bytea,
      slotNo :: bigint,
      blockId :: bytea,
      blockNo :: bigint,
      ARRAY_AGG(payoutTxId) :: bytea[],
      ARRAY_AGG(payoutTxIx) :: smallint[]
    FROM marlowe.withdrawalTxIn
    WHERE txId = $1 :: bytea
    GROUP BY slotNo, blockId, blockNo, txId
  |]

decodeWithdrawal :: (ByteString, Int64, ByteString, Int64, Vector ByteString, Vector Int16) -> Withdrawal
decodeWithdrawal (txId, slot, hash, block, payoutTxIds, payoutTxIxs) = Withdrawal
  { block = decodeBlockHeader slot hash block
  , withdrawnPayouts = Set.fromList $ zipWith decodeTxOutRef (V.toList payoutTxIds) (V.toList payoutTxIxs)
  , withdrawalTx = TxId txId
  }
