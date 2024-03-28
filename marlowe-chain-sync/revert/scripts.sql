-- Revert chain:scripts from pg

BEGIN;

DROP TABLE chain.script;

COMMIT;
