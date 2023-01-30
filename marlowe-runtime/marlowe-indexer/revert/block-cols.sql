-- Revert marlowe:block-cols from pg

BEGIN;

DROP INDEX createTxOut_slotNo;
DROP INDEX createTxOut_slotNo_txId_txIx;

DROP INDEX applyTx_slotNo ON marlowe.applyTx;
DROP INDEX applyTx_slotNo_txId_txIx ON marlowe.applyTx;

DROP INDEX withdrawalTxIn_slotNo ON marlowe.withdrawalTxIn;
DROP INDEX withdrawalTxIn_slotNo_txId_txIx ON marlowe.withdrawalTxIn;
DROP INDEX withdrawalTxIn_createTxId_createTxIx ON marlowe.withdrawalTxIn;

ALTER TABLE marlowe.createTxOut
  DROP COLUMN slotNo,
  DROP COLUMN blockNo;

ALTER TABLE marlowe.applyTx
  DROP COLUMN slotNo,
  DROP COLUMN blockNo;

ALTER TABLE marlowe.withdrawalTxIn
  DROP COLUMN createTxId,
  DROP COLUMN createTxIx,
  DROP COLUMN slotNo,
  DROP COLUMN blockNo;

COMMIT;
