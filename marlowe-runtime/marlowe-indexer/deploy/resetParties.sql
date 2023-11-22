-- Deploy marlowe:resetParties to pg
-- requires: parties

BEGIN;

TRUNCATE TABLE marlowe.contractTxOutPartyAddress;
TRUNCATE TABLE marlowe.contractTxOutPartyRole;

COMMIT;
