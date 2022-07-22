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
  ( txId BYTEA NOT NULL
  , txIx BIGINT NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT pk_assetOut PRIMARY KEY (txId, txIx, assetId)
  , CONSTRAINT fk_assetOut_txOut FOREIGN KEY(txId, txIx) REFERENCES chain.txOut(txId, txIx)
      ON DELETE CASCADE
  , CONSTRAINT fk_assetOut_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

CREATE TABLE chain.assetMint
  ( txId BYTEA NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT pk_assetMint PRIMARY KEY (txId, assetId)
  , CONSTRAINT fk_assetMint_tx FOREIGN KEY(txId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
  , CONSTRAINT fk_assetMint_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

COMMIT;
