-- Revert chain:md5address from pg

BEGIN;

DROP INDEX chain.txOut_address;
CREATE INDEX txOut_address ON chain.txOut USING BTREE (address);

COMMIT;
