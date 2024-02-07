-- Revert chain:scripts from pg

BEGIN;

DROP TABLE chain.script;

DROP TYPE chain.SCRIPTLANG;

COMMIT;
