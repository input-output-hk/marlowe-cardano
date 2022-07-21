-- Deploy chain:block to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.block
  ( id SERIAL
  , hash BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , blockNo BIGINT NOT NULL
  , rollbackBlock INT
  , rollbackToGenesis BOOLEAN
  , CONSTRAINT block_rollback_exclusive CHECK (rollbackBlock IS NULL OR rollbackToGenesis IS NULL)
  , CONSTRAINT pk_block PRIMARY KEY (id)
  , CONSTRAINT unique_block_hash UNIQUE (hash)
  , CONSTRAINT fk_block_block FOREIGN KEY(rollbackBlock) REFERENCES chain.block(id)
  );

CREATE INDEX block_slotNo ON chain.block(slotNo);

COMMIT;
