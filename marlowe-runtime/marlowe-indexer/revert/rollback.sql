-- Revert marlowe:rollback from pg

BEGIN;

ALTER TABLE marlowe.txOut
  DROP CONSTRAINT txOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.txOutAsset
  DROP CONSTRAINT txOutAsset_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.contractTxOut
  DROP CONSTRAINT contractTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.createTxOut
  DROP CONSTRAINT createTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.applyTx
  DROP CONSTRAINT applyTx_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.payoutTxOut
  DROP CONSTRAINT payoutTxOut_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.withdrawalTxIn
  DROP CONSTRAINT withdrawalTxIn_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.invalidApplyTx
  DROP CONSTRAINT invalidApplyTx_blockId_fkey,
  ADD FOREIGN KEY (blockId) REFERENCES marlowe.block (id);

ALTER TABLE marlowe.block
  ADD COLUMN rollbackToBlock BYTEA,
  ADD COLUMN rollbackToSlot BIGINT;

DROP INDEX marlowe.rollbackBlock_toSlotNo;
DROP INDEX marlowe.rollbackBlock_toBlock;

DROP TABLE marlowe.rollbackBlock;

COMMIT;
