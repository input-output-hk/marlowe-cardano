-- Revert marlowe:tag-text from pg

BEGIN;

ALTER TABLE marlowe.contractTxOutTag ALTER COLUMN tag TYPE VARCHAR(64);

COMMIT;
