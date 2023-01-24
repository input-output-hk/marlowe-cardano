{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTipForContract
  where

import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader(..), BlockHeaderHash(..), ChainPoint, TxId(..), TxOutRef(..), WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..))

getTipForContract :: ContractId -> T.Transaction ChainPoint
getTipForContract (ContractId TxOutRef{..}) = T.statement params $ decodePoint <$>
  [maybeStatement|
    WITH contractId (txId, txIx) AS
      ( SELECT $1 :: bytea, $2 :: smallint
      )
    , createBlock AS
      ( SELECT block.*
        FROM marlowe.block
        JOIN marlowe.createTxOut
          ON createTxOut.blockId = block.id
        JOIN contractId USING (txId, txIx)
        WHERE block.rollbackToSlot IS NULL
      )
    , applyBlock AS
      ( SELECT block.*
        FROM marlowe.block
        JOIN marlowe.applyTx
          ON applyTx.blockId = block.id
        JOIN contractId
          ON applyTx.createTxId = contractId.txId
            AND applyTx.createTxIx = contractId.txIx
        WHERE block.rollbackToSlot IS NULL
        ORDER BY block.slotNo DESC
        LIMIT 1
      )
    , withdrawalBlock AS
      ( SELECT block.*
        FROM marlowe.applyTx
        JOIN marlowe.payoutTxOut USING (txId)
        JOIN marlowe.withdrawalTxIn
          ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
            AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
        JOIN marlowe.block
          ON withdrawalTxIn.blockId = block.id
        JOIN contractId
          ON applyTx.createTxId = contractId.txId
            AND applyTx.createTxIx = contractId.txIx
        WHERE block.rollbackToSlot IS NULL
        ORDER BY block.slotNo DESC
        LIMIT 1
      )
      SELECT
        block.slotNo :: bigint,
        block.id :: bytea,
        block.blockNo :: bigint
      FROM
        ( SELECT * FROM createBlock
          UNION SELECT * FROM applyBlock
          UNION SELECT * FROM withdrawalBlock
        ) AS block
      ORDER BY block.slotNo DESC
      LIMIT 1
  |]
  where
    params = (unTxId txId, fromIntegral txIx)
    decodePoint = \case
      Nothing -> Genesis
      Just (slot, hash, block) -> At $ BlockHeader
        (fromIntegral slot)
        (BlockHeaderHash hash)
        (fromIntegral block)
