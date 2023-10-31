{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Cardano where

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley (Tx (ShelleyTx))
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron (byronTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

blockToRows :: BlockInMode CardanoMode -> BlockRowGroup
blockToRows (BlockInMode (Block (BlockHeader slot headerHash block) txs) _) =
  ( BlockRow{..}
  , txRow <$> txs
  )
  where
    hash = serialiseToBytea headerHash
    slotNo = convertSlotNo slot
    blockNo = convertBlockNo block

    txRow :: Tx era -> TxRowGroup
    txRow tx = case tx of
      ByronTx byronTx -> byronTxRow slotNo hash txId byronTx
      ShelleyTx era' shelleyTx -> _
      where
        txId = serialiseToBytea $ getTxId $ getTxBody tx
