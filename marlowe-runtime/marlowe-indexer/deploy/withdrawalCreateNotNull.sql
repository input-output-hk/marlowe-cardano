-- Deploy marlowe:withdrawalCreateNotNull to pg
-- requires: block-cols

BEGIN;

UPDATE marlowe.withdrawalTxIn
  SET
    createTxId = applyTx.createTxId,
    createTxIx = applyTx.createTxIx
  FROM marlowe.payoutTxOut
  JOIN marlowe.applyTx ON payoutTxOut.txId = applyTx.txId
  WHERE payoutTxOut.txId = withdrawalTxIn.payoutTxId
    AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx;

ALTER TABLE marlowe.withdrawalTxIn
  ALTER COLUMN createTxId SET NOT NULL,
  ALTER COLUMN createTxIx SET NOT NULL;

COMMIT;
