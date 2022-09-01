-- Deploy chain:md5address to pg
-- requires: tx

BEGIN;

DROP INDEX chain.txOut_address;
CREATE INDEX txOut_address ON chain.txOut USING BTREE (CAST(md5(address) AS uuid));

COMMIT;
