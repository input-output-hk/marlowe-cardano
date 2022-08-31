-- Revert chain:tx from pg

BEGIN;

DROP TABLE chain.txIn;
DROP TABLE chain.txOut;
DROP TABLE chain.tx;

COMMIT;
