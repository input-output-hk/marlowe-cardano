-- Deploy chain:block to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.block
  ( hash BYTEA NOT NULL
  , slotNo BIGINT
  , blockNo BIGINT
  , rollbackToBlock BYTEA
  , rollbackToGenesis BOOLEAN
  , CONSTRAINT block_rollback_exclusive CHECK (rollbackToBlock IS NULL OR rollbackToGenesis IS NULL)
  , CONSTRAINT pk_block PRIMARY KEY (hash)
  , CONSTRAINT fk_block_block FOREIGN KEY(rollbackToBlock) REFERENCES chain.block(hash)
  );

CREATE INDEX block_hash_slotNo
    ON chain.block
 USING BTREE (hash, slotNo DESC);

CREATE INDEX block_slotNo
    ON chain.block
 USING BRIN (slotNo);

COMMIT;
