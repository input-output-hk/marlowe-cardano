-- Deploy marlowe:tags to pg
-- requires: schema

BEGIN;

CREATE TABLE marlowe.contractTxOutTag
  ( tag VARCHAR(64) NOT NULL
  , txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , PRIMARY KEY (tag, txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.contractTxOut ON DELETE CASCADE
  );

CREATE INDEX contractTxOutTag_tag ON marlowe.contractTxOutTag USING BTREE (tag);

COMMIT;
