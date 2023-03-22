-- Revert marlowe:tags from pg

BEGIN;

DROP TABLE marlowe.contractTxOutTag;

COMMIT;
