-- Deploy chain:block to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.block
  ( id BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , blockNo BIGINT NOT NULL
  , rollbackToBlock BYTEA
  , rollbackToSlot BIGINT
  ) PARTITION BY RANGE (slotNo);

CREATE INDEX block_slotNo ON chain.block USING BRIN (slotNo);
CREATE INDEX block_slotNo_id ON chain.block USING BTREE (slotNo, id);

COMMIT;
