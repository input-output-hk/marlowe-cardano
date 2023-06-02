{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal
  where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId(..), TokenName(..), TxId(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState
  (decodeBlockHeader, decodeContractId, decodeTxOutRef)

getWithdrawal :: TxId -> T.Transaction (Maybe Withdrawal)
getWithdrawal txId = fmap decodeWithdrawal <$> T.statement (unTxId txId)
  [maybeStatement|
    SELECT
      withdrawalTxIn.txId :: bytea,
      withdrawalTxIn.slotNo :: bigint,
      withdrawalTxIn.blockId :: bytea,
      withdrawalTxIn.blockNo :: bigint,
      ARRAY_AGG(withdrawalTxIn.payoutTxId) :: bytea[],
      ARRAY_AGG(withdrawalTxIn.payoutTxIx) :: smallint[],
      ARRAY_AGG(withdrawalTxIn.createTxId) :: bytea[],
      ARRAY_AGG(withdrawalTxIn.createTxIx) :: smallint[],
      ARRAY_AGG(payoutTxOut.rolesCurrency) :: bytea[],
      ARRAY_AGG(payoutTxOut.role) :: bytea[]
    FROM marlowe.withdrawalTxIn
    JOIN marlowe.payoutTxOut
      ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
      AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
    WHERE withdrawalTxIn.txId = $1 :: bytea
    GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
  |]

type ResultRow =
  ( ByteString
  , Int64
  , ByteString
  , Int64
  , Vector ByteString
  , Vector Int16
  , Vector ByteString
  , Vector Int16
  , Vector ByteString
  , Vector ByteString
  )

decodeWithdrawal :: ResultRow -> Withdrawal
decodeWithdrawal (txId, slot, hash, block, payoutTxIds, payoutTxIxs, createTxIds, createTxIxs, roleCurrencies, roles) = Withdrawal
  { block = decodeBlockHeader slot hash block
  , withdrawnPayouts = Map.fromList
      $ V.toList
      $ V.zip (V.zipWith decodeTxOutRef payoutTxIds payoutTxIxs)
      $ V.zipWith6 decodePayoutRef createTxIds createTxIxs payoutTxIds payoutTxIxs roleCurrencies roles
  , withdrawalTx = TxId txId
  }

decodePayoutRef :: ByteString -> Int16 -> ByteString -> Int16 -> ByteString -> ByteString -> PayoutRef
decodePayoutRef createTxId createTxIx txId txIx rolesCurrency role = PayoutRef
  { contractId = decodeContractId createTxId createTxIx
  , payout = decodeTxOutRef txId txIx
  , rolesCurrency = PolicyId rolesCurrency
  , role = TokenName role
  }
