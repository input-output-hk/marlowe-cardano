-- Revert marlowe:parties from pg

BEGIN;

DROP TABLE marlowe.createTxOutPartyAddress;
DROP TABLE marlowe.createTxOutPartyRole;

COMMIT;
