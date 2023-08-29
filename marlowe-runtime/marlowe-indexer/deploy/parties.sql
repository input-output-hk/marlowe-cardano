-- Deploy marlowe:parties to pg
-- requires: schema

BEGIN;

CREATE TABLE marlowe.contractTxOutPartyAddress
  ( address BYTEA NOT NULL
  , txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , createTxId BYTEA NOT NULL
  , createTxIx SMALLINT NOT NULL
  , PRIMARY KEY (address, txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.contractTxOut ON DELETE CASCADE
  , FOREIGN KEY (createTxId, createTxIx) REFERENCES marlowe.createTxOut (txId, txIx) ON DELETE CASCADE
  );

CREATE INDEX contractTxOutPartyAddress_tag ON marlowe.contractTxOutPartyAddress USING BTREE (address);

CREATE TABLE marlowe.contractTxOutPartyRole
  ( rolesCurrency BYTEA NOT NULL
  , role BYTEA NOT NULL
  , txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , createTxId BYTEA NOT NULL
  , createTxIx SMALLINT NOT NULL
  , PRIMARY KEY (rolesCurrency, role, txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.contractTxOut ON DELETE CASCADE
  , FOREIGN KEY (createTxId, createTxIx) REFERENCES marlowe.createTxOut (txId, txIx) ON DELETE CASCADE
  );

CREATE INDEX contractTxOutPartyRole_tag ON marlowe.contractTxOutPartyRole USING BTREE (rolesCurrency, role);

COMMIT;
