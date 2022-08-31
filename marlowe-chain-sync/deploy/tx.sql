-- Deploy chain:tx to pg
-- requires: block

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.tx
  ( id BYTEA NOT NULL
  , blockId BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , validityLowerBound BIGINT
  , validityUpperBound BIGINT
  , metadataKey1564 BYTEA
  , isValid BOOLEAN NOT NULL
  ) PARTITION BY RANGE (slotNo);

CREATE INDEX tx_slotNo ON chain.tx USING BRIN (slotNo);
CREATE INDEX tx_slotNo_blockId ON chain.tx USING BTREE (slotNo, blockId);
CREATE INDEX tx_id ON chain.tx USING BTREE (id);

CREATE TABLE chain.txOut
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , slotNo BIGINT NOT NULL
  , address BYTEA NOT NULL
  , lovelace BIGINT NOT NULL
  , datumHash BYTEA
  , datumBytes BYTEA
  , isCollateral BOOLEAN NOT NULL
  ) PARTITION BY RANGE (slotNo);

CREATE INDEX txOut_slotNo ON chain.txOut USING BRIN (slotNo);
CREATE INDEX txOut_txId ON chain.txOut USING BTREE (txId);
CREATE INDEX txOut_txId_txIx ON chain.txOut USING BTREE (txId, txIx);
CREATE INDEX txOut_address ON chain.txOut USING BTREE (address);

CREATE TABLE chain.txIn
  ( txOutId BYTEA NOT NULL
  , txOutIx SMALLINT NOT NULL
  , txInId BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , redeemerDatumBytes BYTEA
  , isCollateral BOOLEAN NOT NULL
  ) PARTITION BY RANGE (slotNo);

CREATE INDEX txIn_slotNo ON chain.txIn USING BRIN (slotNo);
CREATE INDEX txIn_txId ON chain.txIn USING BTREE (txInId);
CREATE INDEX txIn_txId_txIx ON chain.txIn USING BTREE (txOutId, txOutIx);

COMMIT;
