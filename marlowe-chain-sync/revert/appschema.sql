-- Revert chain:appschema from pg

BEGIN;

DROP SCHEMA chain;

COMMIT;
