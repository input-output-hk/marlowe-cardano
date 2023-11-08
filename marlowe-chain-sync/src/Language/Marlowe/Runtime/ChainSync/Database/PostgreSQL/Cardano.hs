{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Cardano where

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley (Tx (ShelleyTx))
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Allegra (allegraTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron (byronTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Conway (conwayTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (shelleyTxRow)
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
      ShelleyTx shelleyEra shelleyTx -> case shelleyEra of
        ShelleyBasedEraShelley -> shelleyTxRow slotNo hash txId shelleyTx
        ShelleyBasedEraAllegra -> allegraTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraMary -> maryTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraAlonzo -> alonzoTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraBabbage -> babbageTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraConway -> conwayTxToRows slotNo hash txId shelleyTx
      where
        txId = serialiseToBytea $ getTxId $ getTxBody tx
