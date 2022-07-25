-- Deploy chain:block to pg
-- requires: appschema

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.block
  ( id BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , blockNo BIGINT NOT NULL
  , rollbackToBlock BYTEA
  , rollbackToSlot BYTEA
  ) PARTITION BY RANGE (slotNo);

CREATE INDEX block_slotNo ON chain.block USING BRIN (slotNo);
CREATE INDEX block_slotNo_id ON chain.block USING BTREE (slotNo, id);

CREATE TABLE chain.block_0
PARTITION OF chain.block
FOR VALUES FROM (-1) TO (10000000);

CREATE TABLE chain.block_1m
PARTITION OF chain.block
FOR VALUES FROM (10000000) TO (20000000);

CREATE TABLE chain.block_2m
PARTITION OF chain.block
FOR VALUES FROM (20000000) TO (30000000);

CREATE TABLE chain.block_3m
PARTITION OF chain.block
FOR VALUES FROM (30000000) TO (40000000);

CREATE TABLE chain.block_4m
PARTITION OF chain.block
FOR VALUES FROM (40000000) TO (50000000);

CREATE TABLE chain.block_5m
PARTITION OF chain.block
FOR VALUES FROM (50000000) TO (60000000);

CREATE TABLE chain.block_6m
PARTITION OF chain.block
FOR VALUES FROM (60000000) TO (70000000);

CREATE TABLE chain.block_7m
PARTITION OF chain.block
FOR VALUES FROM (70000000) TO (80000000);

CREATE TABLE chain.block_8m
PARTITION OF chain.block
FOR VALUES FROM (80000000) TO (90000000);

CREATE TABLE chain.block_9m
PARTITION OF chain.block
FOR VALUES FROM (90000000) TO (100000000);

CREATE TABLE chain.block_10m
PARTITION OF chain.block
FOR VALUES FROM (100000000) TO (110000000);

COMMIT;
