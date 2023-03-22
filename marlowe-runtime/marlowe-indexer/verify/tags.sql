-- Verify marlowe:tags on pg

BEGIN;

SELECT tag, txId, txIx FROM marlowe.contractTxOutTag WHERE TRUE;

ROLLBACK;
