-- Deploy chain:asset to pg
-- requires: tx

BEGIN;

CREATE TABLE chain.asset
  ( id SERIAL
  , policyId BYTEA NOT NULL
  , name BYTEA NOT NULL
  , CONSTRAINT pk_asset PRIMARY KEY (id)
  , CONSTRAINT unique_asset UNIQUE (policyId, name)
  );

CREATE TABLE chain.assetOut
  ( txOutId BYTEA NOT NULL
  , txOutIx SMALLINT NOT NULL
  , slotNo BIGINT NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT fk_assetOut_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

CREATE INDEX assetOut_slotNo ON chain.assetOut USING BTREE (slotNo);
CREATE INDEX assetOut_txId_txIx ON chain.assetOut USING BTREE (txOutId, txOutIx);
CREATE INDEX assetOut_assetId ON chain.assetOut USING BTREE (assetId);

CREATE TABLE chain.assetMint
  ( txId BYTEA NOT NULL
  , slotNo BIGINT NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT fk_assetMint_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

COMMIT;

CREATE INDEX assetMint_slotNo ON chain.assetMint USING BTREE (slotNo);
CREATE INDEX assetMint_txId ON chain.assetMint USING BTREE (txId);
CREATE INDEX assetMint_assetId ON chain.assetMint USING BTREE (assetId);

