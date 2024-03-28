{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Cardano where

import Cardano.Api
import Cardano.Api.Block (Block (..))
import Cardano.Api.Shelley (Tx (ShelleyTx))
import qualified Cardano.Chain.Block as LB
import Cardano.Chain.UTxO (ATxAux (aTaTx), ATxPayload (aUnTxPayload), taTx)
import Cardano.Crypto.Hashing (abstractHashToBytes, hashDecoded)
import Data.ByteString (ByteString)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Allegra (allegraTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Alonzo (alonzoTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Babbage (babbageTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron (byronTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Conway (conwayTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Mary (maryTxToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley (shelleyTxRow)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types
import qualified Ouroboros.Consensus.Byron.Ledger as C

blockToRows :: BlockInMode -> BlockRowGroup
blockToRows (BlockInMode _ block) =
  ( BlockRow{..}
  , txGroups
  )
  where
    txGroups = case block of
      ByronBlock (C.ByronBlock (LB.ABOBBlock (LB.ABlock _ body _)) _ _) ->
        (\tx -> byronTxRow slotNo hash (byronTxId tx) $ taTx tx) <$> aUnTxPayload (LB.bodyTxPayload body)
      ByronBlock C.ByronBlock{} -> []
      _ -> case block of
        Block _ txs -> txRow <$> txs
    BlockHeader slot headerHash blockNo' = getBlockHeader block
    hash = serialiseToBytea headerHash
    slotNo = convertSlotNo slot
    blockNo = convertBlockNo blockNo'

    txRow :: Tx era -> TxRowGroup
    txRow tx = case tx of
      ShelleyTx shelleyEra shelleyTx -> case shelleyEra of
        ShelleyBasedEraShelley -> shelleyTxRow slotNo hash txId shelleyTx
        ShelleyBasedEraAllegra -> allegraTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraMary -> maryTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraAlonzo -> alonzoTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraBabbage -> babbageTxToRows slotNo hash txId shelleyTx
        ShelleyBasedEraConway -> conwayTxToRows slotNo hash txId shelleyTx
      where
        txId = serialiseToBytea $ getTxId $ getTxBody tx

byronTxId :: ATxAux ByteString -> Bytea
byronTxId = Bytea . abstractHashToBytes . hashDecoded . aTaTx
