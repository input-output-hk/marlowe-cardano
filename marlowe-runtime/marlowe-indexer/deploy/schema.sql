-- Deploy marlowe:schema to pg

BEGIN;

-- Create the application schema
CREATE SCHEMA marlowe;

-- Create the block table
CREATE TABLE marlowe.block
  ( id BYTEA PRIMARY KEY
  , slotNo BIGINT NOT NULL
  , blockNo BIGINT NOT NULL
  , rollbackToBlock BYTEA
  , rollbackToSlot BIGINT
  );

CREATE INDEX block_slotNo ON marlowe.block USING BTREE (slotNo);
CREATE INDEX block_slotNo_id ON marlowe.block USING BTREE (slotNo, id);

-- Create the txOut table
CREATE TABLE marlowe.txOut
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , address BYTEA NOT NULL
  , lovelace BIGINT NOT NULL
  , PRIMARY KEY (txId, txIx)
  );

CREATE INDEX txOut_address ON marlowe.txOut USING BTREE (CAST(md5(address) AS uuid));

-- Create the txOutAsset table
CREATE TABLE marlowe.txOutAsset
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , policyId BYTEA NOT NULL
  , name BYTEA NOT NULL
  , quantity BIGINT NOT NULL
  , PRIMARY KEY (txId, txIx, policyId, name)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.txOut
  );

CREATE INDEX txOutAsset_txId_txIx ON marlowe.txOutAsset USING BTREE (txId, txIx);

-- Create the contractTxOut table
CREATE TABLE marlowe.contractTxOut
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , contract BYTEA NOT NULL
  , state BYTEA NOT NULL
  , rolesCurrency BYTEA NOT NULL
  , PRIMARY KEY (txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.txOut
  );

-- Create the createTxOut table
CREATE TABLE marlowe.createTxOut
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , blockId BYTEA NOT NULL REFERENCES marlowe.block (id)
  , payoutScriptHash BYTEA NOT NULL
  , metadata BYTEA
  , PRIMARY KEY (txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.contractTxOut
  );

CREATE INDEX createTxOut_blockId ON marlowe.createTxOut USING BTREE (blockId);

-- Create the applyTx table
CREATE TABLE marlowe.applyTx
  ( txId BYTEA PRIMARY KEY
  , createTxId BYTEA NOT NULL
  , createTxIx SMALLINT NOT NULL
  , blockId BYTEA NOT NULL REFERENCES marlowe.block (id)
  , invalidBefore TIMESTAMP NOT NULL
  , invalidHereafter TIMESTAMP NOT NULL
  , metadata BYTEA
  , inputTxId BYTEA NOT NULL
  , inputTxIx SMALLINT NOT NULL
  , inputs BYTEA NOT NULL
  , outputTxIx SMALLINT
  , FOREIGN KEY (createTxId, createTxIx) REFERENCES marlowe.createTxOut (txId, txIx)
  , FOREIGN KEY (txId, outputTxIx) REFERENCES marlowe.contractTxOut (txId, txIx)
  , FOREIGN KEY (inputTxId, inputTxIx) REFERENCES marlowe.contractTxOut (txId, txIx)
  , UNIQUE (inputTxId, inputTxIx)
  );

CREATE INDEX applyTx_blockId ON marlowe.applyTx USING BTREE (blockId);

-- Create the payoutTxOut table
CREATE TABLE marlowe.payoutTxOut
  ( txId BYTEA NOT NULL
  , txIx SMALLINT NOT NULL
  , role BYTEA NOT NULL
  , PRIMARY KEY (txId, txIx)
  , FOREIGN KEY (txId, txIx) REFERENCES marlowe.txOut
  , FOREIGN KEY (txId) REFERENCES marlowe.applyTx
  );

CREATE INDEX payoutTxOut_txId ON marlowe.payoutTxOut USING BTREE (txId);

-- Create the withdrawalTxIn table
CREATE TABLE marlowe.withdrawalTxIn
  ( txId BYTEA NOT NULL
  , blockId BYTEA NOT NULL REFERENCES marlowe.block (id)
  , payoutTxId BYTEA NOT NULL
  , payoutTxIx SMALLINT NOT NULL
  , PRIMARY KEY (payoutTxId, payoutTxIx)
  , FOREIGN KEY (payoutTxId, payoutTxIx) REFERENCES marlowe.payoutTxOut (txId, txIx)
  , UNIQUE (payoutTxId, payoutTxIx)
  );

CREATE INDEX withdrawTxIn_blockId ON marlowe.withdrawalTxIn USING BTREE (blockId);
CREATE INDEX withdrawTxIn_txId ON marlowe.withdrawalTxIn USING BTREE (txId);

COMMIT;
