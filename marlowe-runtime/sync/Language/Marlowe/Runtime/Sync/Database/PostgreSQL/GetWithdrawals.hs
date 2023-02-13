{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawals
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Vector as V
import Hasql.TH (maybeStatement, vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId(..), TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (decodeWithdrawal)

getWithdrawals :: ContractId -> T.Transaction (Maybe [Withdrawal])
getWithdrawals (ContractId TxOutRef{..}) = runMaybeT do
  let params = (unTxId txId, fromIntegral txIx)
  _ <- MaybeT $ T.statement params
    [maybeStatement|
      SELECT txId :: bytea
      FROM marlowe.createTxOut
      WHERE txId = $1 :: bytea
        AND txIx = $2 :: smallint
    |]
  lift $ V.toList . fmap decodeWithdrawal <$> T.statement params
    [vectorStatement|
      SELECT
        txId :: bytea,
        slotNo :: bigint,
        blockId :: bytea,
        blockNo :: bigint,
        ARRAY_AGG(payoutTxId) :: bytea[],
        ARRAY_AGG(payoutTxIx) :: smallint[]
      FROM marlowe.withdrawalTxIn
      WHERE createTxId = $1 :: bytea
        AND createTxIx = $2 :: smallint
      GROUP BY slotNo, blockId, blockNo, txId
      ORDER BY slotNo, blockId, blockNo, txId
    |]
