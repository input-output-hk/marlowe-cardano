-- Deploy chain:scripts to pg
-- requires: appschema

BEGIN;

-- NOTE this migration requires a reset of the entire database, as the chain
-- needs to be re-synchronized.
TRUNCATE chain.block CASCADE;
TRUNCATE chain.tx CASCADE;
TRUNCATE chain.txIn CASCADE;
TRUNCATE chain.txOut CASCADE;
TRUNCATE chain.assetOut;
TRUNCATE chain.assetMint;

CREATE TABLE chain.script
  ( id BYTEA PRIMARY KEY
  , bytes BYTEA NOT NULL
  );

COMMIT;
