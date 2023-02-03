-- Deploy marlowe:rollback to pg
-- requires: schema

BEGIN;

CREATE TABLE marlowe.rollbackBlock
  ( fromBlock BYTEA PRIMARY KEY
  , toBlock BYTEA NOT NULL REFERENCES marlowe.block (id)
  , toSlotNo BIGINT NOT NULL
  );

CREATE INDEX rollbackBlock_toSlotNo ON marlowe.rollbackBlock USING BTREE (toSlotNo);
CREATE INDEX rollbackBlock_toBlock ON marlowe.rollbackBlock USING BTREE (toBlock);

DELETE FROM marlowe.block
WHERE rollbackToBlock IS NOT NULL
  AND rollbackToSlot IS NOT NULL;

ALTER TABLE marlowe.block
  DROP COLUMN rollbackToBlock,
  DROP COLUMN rollbackToSlot;

ALTER TABLE marlowe.txOut
  DROP CONSTRAINT txOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.txOutAsset
  DROP CONSTRAINT txOutAsset_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.contractTxOut
  DROP CONSTRAINT contractTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.createTxOut
  DROP CONSTRAINT createTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.applyTx
  DROP CONSTRAINT applyTx_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.payoutTxOut
  DROP CONSTRAINT payoutTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.withdrawalTxIn
  DROP CONSTRAINT withdrawalTxIn_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

ALTER TABLE marlowe.invalidApplyTx
  DROP CONSTRAINT invalidApplyTx_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id) ON DELETE CASCADE;

COMMIT;
