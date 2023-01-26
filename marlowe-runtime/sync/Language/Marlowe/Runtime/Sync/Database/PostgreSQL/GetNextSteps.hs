{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps
  where

import qualified Data.Vector as V
import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader(..), BlockHeaderHash(..), ChainPoint, TxId(..), TxOutRef(..), WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), MarloweVersionTag(V1), Transaction)
import Language.Marlowe.Runtime.History.Api (ContractStep(..), RedeemStep)
import Language.Marlowe.Runtime.Sync.Database (NextSteps(..))

getNextSteps :: ContractId -> ChainPoint -> T.Transaction NextSteps
getNextSteps contractId point = do
  orient point >>= \case
    RolledBack toPoint -> pure $ Rollback toPoint
    AtTip -> pure Wait
    BeforeTip -> getNextTxIds contractId point >>= \case
      Nothing -> pure Wait
      Just NextTxIds{..} -> do
        applySteps <- getApplySteps nextBlock nextApplyTxIds
        redeemSteps <- getRedeemSteps nextBlock nextWithdrawalTxIds
        pure $ Next MarloweV1 nextBlock $ (ApplyTransaction <$> applySteps) <> (RedeemPayout <$> redeemSteps)

data Orientation
  = BeforeTip
  | AtTip
  | RolledBack ChainPoint

orient :: ChainPoint -> T.Transaction Orientation
orient Genesis = pure BeforeTip
orient (At BlockHeader{..}) = T.statement (unBlockHeaderHash headerHash) $ decodeResult <$>
  [maybeStatement|
    SELECT
      rollbackBlock.slotNo :: bigint?,
      rollbackBlock.id :: bytea?,
      rollbackBlock.blockNo :: bigint?
    FROM marlowe.block
    LEFT JOIN marlowe.block AS rollbackBlock ON block.rollbackToBlock = rollbackBlock.id
    WHERE block.id = $1 :: bytea
  |]
  where
    decodeResult Nothing = RolledBack Genesis
    decodeResult (Just (Just slot, Just hash, Just block)) = RolledBack $ At BlockHeader
      { slotNo = fromIntegral slot
      , headerHash = BlockHeaderHash hash
      , blockNo = fromIntegral block
      }
    decodeResult _ = BeforeTip

data NextTxIds = NextTxIds
  { nextBlock :: BlockHeader
  , nextApplyTxIds :: [TxId]
  , nextWithdrawalTxIds :: [TxId]
  }

getNextTxIds :: ContractId -> ChainPoint -> T.Transaction (Maybe NextTxIds)
getNextTxIds (ContractId TxOutRef{..}) point = T.statement params $ fmap (NextTxIds <$> decodeBlockHeader <*> decodeApplyTxIds <*> decodeWithdrawalTxIds) <$>
  [maybeStatement|
    WITH params (createTxId, createTxIx, afterSlot) AS
      ( SELECT $1 :: bytea, $2 :: smallint, $3 :: bigint
      )
    , nextApplyTxIds (slotNo, blockHeaderHash, blockNo, txIds) AS
      ( SELECT
          block.slotNo,
          (ARRAY_AGG(block.id))[1],
          (ARRAY_AGG(block.blockNo))[1],
          ARRAY_AGG(applyTx.txId)
        FROM marlowe.applyTx
        JOIN marlowe.block
          ON block.id = applyTx.blockId
        JOIN params USING (createTxId, createTxIx)
        WHERE block.rollbackToBlock IS NULL
          AND block.slotNo > params.afterSlot
        GROUP BY block.slotNo
        ORDER BY block.slotNo
        LIMIT 1
      )
    , nextWithdrawalTxIds (slotNo, blockHeaderHash, blockNo, txIds) AS
      ( SELECT
          block.slotNo,
          (ARRAY_AGG(block.id))[1],
          (ARRAY_AGG(block.blockNo))[1],
          ARRAY_AGG(withdrawalTxIn.txId)
        FROM marlowe.withdrawalTxIn
        JOIN marlowe.block
          ON block.id = withdrawalTxIn.blockId
        JOIN marlowe.payoutTxOut
          ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
          AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
        JOIN marlowe.applyTx
          ON applyTx.txId = payoutTxOut.txId
        JOIN params USING (createTxId, createTxIx)
        WHERE block.rollbackToBlock IS NULL
          AND block.slotNo > params.afterSlot
        GROUP BY block.slotNo
        ORDER BY block.slotNo
        LIMIT 1
      )
    SELECT
      COALESCE (nextApplyTxIds.slotNo, nextWithdrawalTxIds.slotNo) :: bigint,
      COALESCE (nextApplyTxIds.blockHeaderHash, nextWithdrawalTxIds.blockHeaderHash) :: bytea,
      COALESCE (nextApplyTxIds.blockNo, nextWithdrawalTxIds.blockNo) :: bigint,
      COALESCE (nextApplyTxIds.txIds, '{}') :: bytea[],
      COALESCE (nextWithdrawalTxIds.txIds, '{}') :: bytea[]
    FROM nextApplyTxIds
    FULL OUTER JOIN nextWithdrawalTxIds
    ON nextApplyTxIds.slotNo = nextWithdrawalTxIds.slotNo
    ORDER BY COALESCE (nextApplyTxIds.slotNo, nextWithdrawalTxIds.slotNo)
    LIMIT 1
  |]
  where
    params = (unTxId txId, fromIntegral txIx, pointSlot)
    pointSlot = case point of
      Genesis -> -1
      At BlockHeader{..} -> fromIntegral slotNo
    decodeBlockHeader (slot, hash, block, _, _) = BlockHeader
      { slotNo = fromIntegral slot
      , headerHash = BlockHeaderHash hash
      , blockNo = fromIntegral block
      }
    decodeApplyTxIds (_, _, _, ids, _) = decodeTxIds ids
    decodeWithdrawalTxIds (_, _, _, _, ids) = decodeTxIds ids
    decodeTxIds ids = V.toList $ V.map TxId ids

getRedeemSteps :: BlockHeader -> [TxId] -> T.Transaction [RedeemStep 'V1]
getRedeemSteps = error "not implemented"

getApplySteps :: BlockHeader -> [TxId] -> T.Transaction [Transaction 'V1]
getApplySteps = error "not implemented"
