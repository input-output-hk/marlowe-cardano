-- Deploy chain:tx to pg
-- requires: block

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.tx
  ( id SERIAL
  , blockId INT NOT NULL
  , hash BYTEA NOT NULL
  , validityLowerBound BIGINT
  , validityUpperBound BIGINT
  , metadataKey1564 BYTEA
  , CONSTRAINT pk_tx PRIMARY KEY (id)
  , CONSTRAINT unique_tx_hash UNIQUE (hash)
  , CONSTRAINT fk_tx_block FOREIGN KEY(blockId) REFERENCES chain.block(id)
      ON DELETE CASCADE
  );

CREATE TABLE chain.txOut
  ( id SERIAL
  , txId INT NOT NULL
  , ix BIGINT NOT NULL
  , address BYTEA NOT NULL
  , lovelace BIGINT NOT NULL
  , datumHash BYTEA
  , datumBytes BYTEA
  , CONSTRAINT pk_txOut PRIMARY KEY (id)
  , CONSTRAINT unique_txOut UNIQUE (txId, ix)
  , CONSTRAINT fk_txOut_tx FOREIGN KEY(txId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
  );

CREATE INDEX txOut_address ON chain.txOut(address);

CREATE TABLE chain.txIn
  ( id SERIAL
  , txId INT NOT NULL
  , txOutId INT NOT NULL
  , redeemerDatumBytes BYTEA
  , CONSTRAINT pk_txIn PRIMARY KEY (id)
  , CONSTRAINT unique_txIn_txOutId UNIQUE (txOutId)
  , CONSTRAINT fk_txIn_tx FOREIGN KEY(txId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
  , CONSTRAINT fk_txIn_txOut FOREIGN KEY(txOutId) REFERENCES chain.txOut(id)
  );

COMMIT;
