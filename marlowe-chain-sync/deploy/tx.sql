-- Deploy chain:tx to pg
-- requires: block

BEGIN;

SET client_min_messages = 'warning';

CREATE TABLE chain.tx
  ( id BYTEA NOT NULL
  , blockHash BYTEA NOT NULL
  , validityLowerBound BIGINT
  , validityUpperBound BIGINT
  , metadataKey1564 BYTEA
  , CONSTRAINT pk_tx PRIMARY KEY (id)
  , CONSTRAINT fk_tx_block FOREIGN KEY(blockHash) REFERENCES chain.block(hash)
      ON DELETE CASCADE
      DEFERRABLE INITIALLY DEFERRED
  );

CREATE TABLE chain.txOut
  ( txId BYTEA NOT NULL
  , txIx BIGINT NOT NULL
  , address BYTEA NOT NULL
  , lovelace BIGINT NOT NULL
  , datumHash BYTEA
  , datumBytes BYTEA
  , CONSTRAINT pk_txOut PRIMARY KEY (txId, txIx)
  , CONSTRAINT fk_txOut_tx FOREIGN KEY(txId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
      DEFERRABLE INITIALLY DEFERRED
  );

CREATE INDEX txOut_address ON chain.txOut(address);

CREATE TABLE chain.txIn
  ( txOutId BYTEA NOT NULL
  , txOutIx BIGINT NOT NULL
  , txInId BYTEA NOT NULL
  , redeemerDatumBytes BYTEA
  , CONSTRAINT pk_txIn PRIMARY KEY (txOutId, txOutIx, txInId)
  , CONSTRAINT unique_txIn UNIQUE (txOutId, txOutIx)
  , CONSTRAINT fk_txIn_tx FOREIGN KEY(txInId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
      DEFERRABLE INITIALLY DEFERRED
  , CONSTRAINT fk_txIn_txOut FOREIGN KEY(txOutId, txOutIx) REFERENCES chain.txOut(txId, txIx)
      ON DELETE RESTRICT
      DEFERRABLE INITIALLY DEFERRED
  );

COMMIT;
