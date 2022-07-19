-- Deploy chain:asset to pg
-- requires: tx

BEGIN;

CREATE TABLE chain.asset
  ( id SERIAL
  , policyId BYTEA NOT NULL
  , name TEXT NOT NULL
  , CONSTRAINT pk_asset PRIMARY KEY (id)
  , CONSTRAINT unique_asset UNIQUE (policyId, name)
  );

CREATE TABLE chain.assetOut
  ( id SERIAL
  , txOutId INT NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT pk_assetOut PRIMARY KEY (id)
  , CONSTRAINT unique_assetOut UNIQUE (txOutId, assetId)
  , CONSTRAINT fk_assetOut_txOut FOREIGN KEY(txOutId) REFERENCES chain.txOut(id)
      ON DELETE CASCADE
  , CONSTRAINT fk_assetOut_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

CREATE TABLE chain.assetMint
  ( id SERIAL
  , txId INT NOT NULL
  , assetId INT NOT NULL
  , quantity BIGINT NOT NULL
  , CONSTRAINT pk_assetMint PRIMARY KEY (id)
  , CONSTRAINT unique_assetMint UNIQUE (txId, assetId)
  , CONSTRAINT fk_assetMint_tx FOREIGN KEY(txId) REFERENCES chain.tx(id)
      ON DELETE CASCADE
  , CONSTRAINT fk_assetMint_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id)
      ON DELETE RESTRICT
  );

COMMIT;
