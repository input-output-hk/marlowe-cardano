-- Deploy marlowe:tag-text to pg
-- requires: tags

BEGIN;

ALTER TABLE marlowe.contractTxOutTag ALTER COLUMN tag TYPE TEXT;

COMMIT;
