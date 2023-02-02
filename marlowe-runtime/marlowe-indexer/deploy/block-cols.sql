-- Deploy marlowe:block-cols to pg
-- requires: schema

BEGIN;

ALTER TABLE marlowe.createTxOut
  ADD COLUMN slotNo BIGINT,
  ADD COLUMN blockNo BIGINT;

ALTER TABLE marlowe.applyTx
  ADD COLUMN slotNo BIGINT,
  ADD COLUMN blockNo BIGINT;

ALTER TABLE marlowe.withdrawalTxIn
  ADD COLUMN createTxId BYTEA,
  ADD COLUMN createTxIx SMALLINT,
  ADD COLUMN slotNo BIGINT,
  ADD COLUMN blockNo BIGINT;

UPDATE marlowe.createTxOut
  SET
    slotNo = block.slotNo,
    blockNo = block.blockNo
  FROM marlowe.block
  WHERE block.id = createTxOut.blockId;

UPDATE marlowe.applyTx
  SET
    slotNo = block.slotNo,
    blockNo = block.blockNo
  FROM marlowe.block
  WHERE block.id = applyTx.blockId;

UPDATE marlowe.withdrawalTxIn
  SET
    slotNo = block.slotNo,
    blockNo = block.blockNo
  FROM marlowe.block
  WHERE block.id = withdrawalTxIn.blockId;

UPDATE marlowe.withdrawalTxIn
  SET
    createTxId = applyTx.createTxId,
    createTxIx = applyTx.createTxIx
  FROM marlowe.payoutTxOut
  JOIN marlowe.applyTx ON payoutTxOut.txId = applyTx.txId
  WHERE payoutTxOut.txId = withdrawalTxIn.payoutTxId
    AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx;

ALTER TABLE marlowe.createTxOut
  ALTER COLUMN slotNo SET NOT NULL,
  ALTER COLUMN blockNo SET NOT NULL;

ALTER TABLE marlowe.applyTx
  ALTER COLUMN slotNo SET NOT NULL,
  ALTER COLUMN blockNo SET NOT NULL;

ALTER TABLE marlowe.withdrawalTxIn
  ALTER COLUMN slotNo SET NOT NULL,
  ALTER COLUMN blockNo SET NOT NULL,
  ADD FOREIGN KEY (createTxId, createTxIx) REFERENCES marlowe.createTxOut (txId, txIx);

CREATE INDEX createTxOut_slotNo ON marlowe.createTxOut (slotNo);
CREATE INDEX createTxOut_slotNo_txId_txIx ON marlowe.createTxOut (slotNo, txId, txIx);

CREATE INDEX applyTx_slotNo ON marlowe.applyTx (slotNo);
CREATE INDEX applyTx_slotNo_txId_txIx ON marlowe.applyTx (slotNo, txId);

CREATE INDEX withdrawalTxIn_slotNo ON marlowe.withdrawalTxIn (slotNo);
CREATE INDEX withdrawalTxIn_slotNo_txId_txIx ON marlowe.withdrawalTxIn (slotNo, txId);
CREATE INDEX withdrawalTxIn_createTxId_createTxIx ON marlowe.withdrawalTxIn (createTxId, createTxIx);

COMMIT;
