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
  , CONSTRAINT pk_block PRIMARY KEY (id)
  , CONSTRAINT unique_hash UNIQUE (hash)
  , CONSTRAINT fk_block FOREIGN KEY(rollbackBlock) REFERENCES chain.block(id)
  );

CREATE INDEX blockSlotNo ON chain.block(slotNo);

COMMIT;
