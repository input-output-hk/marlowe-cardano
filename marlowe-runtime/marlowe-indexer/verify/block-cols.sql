-- Verify marlowe:block-cols on pg

BEGIN;

SELECT slotNo, blockNo FROM marlowe.createTxOut;
SELECT slotNo, blockNo FROM marlowe.applyTx;
SELECT slotNo, blockNo, createTxId, createTxIx FROM marlowe.withdrawalTxIn;

ROLLBACK;
