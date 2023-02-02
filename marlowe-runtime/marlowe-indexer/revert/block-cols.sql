-- Revert marlowe:block-cols from pg

BEGIN;

DROP INDEX marlowe.createTxOut_slotNo;
DROP INDEX marlowe.createTxOut_slotNo_txId_txIx;

DROP INDEX marlowe.applyTx_slotNo;
DROP INDEX marlowe.applyTx_slotNo_txId_txIx;

DROP INDEX marlowe.withdrawalTxIn_slotNo;
DROP INDEX marlowe.withdrawalTxIn_slotNo_txId_txIx;
DROP INDEX marlowe.withdrawalTxIn_createTxId_createTxIx;

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
