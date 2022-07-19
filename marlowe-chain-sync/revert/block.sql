-- Revert chain:block from pg

BEGIN;

DROP TABLE chain.block CASCADE;

COMMIT;
