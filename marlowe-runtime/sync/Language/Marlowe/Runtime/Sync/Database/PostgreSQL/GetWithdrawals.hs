{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawals
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Hasql.TH (foldStatement, maybeStatement, singletonStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId(..), TxId(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders (foldPage)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (ResultRow, decodeWithdrawal)
import Prelude hiding (init)

getWithdrawals :: WithdrawalFilter -> Range TxId -> T.Transaction (Maybe (Page TxId Withdrawal))
getWithdrawals _ Range{..}
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing

getWithdrawals wFilter@WithdrawalFilter{..} Range{rangeStart = Just txId, ..} = runMaybeT do
  pivot <- MaybeT if Set.null roleCurrencies
    then T.statement (unTxId txId)
      [maybeStatement|
        SELECT slotNo :: bigint, txId :: bytea
        FROM marlowe.withdrawalTxIn
        WHERE txId = $1 :: bytea
      |]
    else T.statement (unTxId txId, V.fromList $ unPolicyId <$> Set.toList roleCurrencies)
      [maybeStatement|
        SELECT withdrawalTxIn.slotNo :: bigint, withdrawalTxIn.txId :: bytea
        FROM marlowe.withdrawalTxIn
        JOIN marlowe.payoutTxOut
          ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
          AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
        JOIN (SELECT UNNEST($2 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE withdrawalTxIn.txId = $1 :: bytea
      |]
  totalCount <- lift $ getTotalCount wFilter
  lift $ getWithdrawalsFrom wFilter totalCount pivot rangeOffset rangeLimit rangeDirection

getWithdrawals wFilter@WithdrawalFilter{..} Range{..} = Just <$> do
  totalCount <- getTotalCount wFilter
  case (Set.null roleCurrencies, rangeDirection) of
    (True, Descending) -> T.statement nullFilterParams $
      [foldStatement|
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
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo DESC, withdrawalTxIn.blockId DESC, withdrawalTxIn.blockNo DESC, withdrawalTxIn.txId DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal rangeLimit rangeDirection totalCount)

    (True, Ascending) -> T.statement nullFilterParams $
      [foldStatement|
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
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal rangeLimit rangeDirection totalCount)

    (False, Descending) -> T.statement nonNullFilterParams $
      [foldStatement|
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
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo DESC, withdrawalTxIn.blockId DESC, withdrawalTxIn.blockNo DESC, withdrawalTxIn.txId DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal rangeLimit rangeDirection totalCount)

    (False, Ascending) -> T.statement nonNullFilterParams $
      [foldStatement|
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
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal rangeLimit rangeDirection totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    nullFilterParams = (fromIntegral rangeOffset, fromIntegral rangeLimit + 1)
    nonNullFilterParams =
      ( fromIntegral rangeOffset
      , fromIntegral rangeLimit + 1
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      )

getTotalCount :: WithdrawalFilter -> T.Transaction Int
getTotalCount WithdrawalFilter{..} = fromIntegral <$> if Set.null roleCurrencies
  then T.statement ()
    [singletonStatement|
      SELECT COUNT(*) :: int
      FROM
        ( SELECT txId
          FROM marlowe.withdrawalTxIn
          GROUP BY txId
        ) AS grouped
    |]
  else T.statement (V.fromList $ unPolicyId <$> Set.toList roleCurrencies)
    [singletonStatement|
      SELECT COUNT(*) :: int
      FROM
        ( SELECT withdrawalTxIn.txId
          FROM marlowe.withdrawalTxIn
          JOIN marlowe.payoutTxOut
            ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
            AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
          JOIN (SELECT UNNEST($1 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
          GROUP BY withdrawalTxIn.txId
        ) AS grouped
    |]

getWithdrawalsFrom
  :: WithdrawalFilter
  -> Int
  -> (Int64, ByteString)
  -> Int
  -> Int
  -> Order
  -> T.Transaction (Page TxId Withdrawal)
getWithdrawalsFrom WithdrawalFilter{..} totalCount (pivotSlot, pivotTxId) offset limit order =
  case (Set.null roleCurrencies, order) of
    (True, Descending) -> T.statement nullFilterParams $
      [foldStatement|
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
        WHERE withdrawalTxIn.slotNo < $1 :: bigint OR (withdrawalTxIn.slotNo = $1 :: bigint AND withdrawalTxIn.txId <= $2 :: bytea)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo DESC, withdrawalTxIn.blockId DESC, withdrawalTxIn.blockNo DESC, withdrawalTxIn.txId DESC
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal limit order totalCount)

    (True, Ascending) -> T.statement nullFilterParams $
      [foldStatement|
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
        WHERE withdrawalTxIn.slotNo > $1 :: bigint OR (withdrawalTxIn.slotNo = $1 :: bigint AND withdrawalTxIn.txId >= $2 :: bytea)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal limit order totalCount)

    (False, Descending) -> T.statement nonNullFilterParams $
      [foldStatement|
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
        JOIN (SELECT UNNEST($5 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE withdrawalTxIn.slotNo < $1 :: bigint OR (withdrawalTxIn.slotNo = $1 :: bigint AND withdrawalTxIn.txId <= $2 :: bytea)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo DESC, withdrawalTxIn.blockId DESC, withdrawalTxIn.blockNo DESC, withdrawalTxIn.txId DESC
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal limit order totalCount)

    (False, Ascending) -> T.statement nonNullFilterParams $
      [foldStatement|
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
        JOIN (SELECT UNNEST($5 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE withdrawalTxIn.slotNo > $1 :: bigint OR (withdrawalTxIn.slotNo = $1 :: bigint AND withdrawalTxIn.txId >= $2 :: bytea)
        GROUP BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        ORDER BY withdrawalTxIn.slotNo, withdrawalTxIn.blockId, withdrawalTxIn.blockNo, withdrawalTxIn.txId
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage decodeTxId decodeWithdrawal limit order totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    nullFilterParams = (pivotSlot, pivotTxId, fromIntegral offset, fromIntegral limit + 1)
    nonNullFilterParams =
      ( pivotSlot
      , pivotTxId
      , fromIntegral offset
      , fromIntegral limit + 1
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      )

decodeTxId :: ResultRow -> TxId
decodeTxId (txId, _, _, _, _, _, _, _, _, _) = TxId txId
