-- Revert chain:drop-assets from pg

BEGIN;

CREATE TABLE chain.asset
  ( id SERIAL
  , policyId BYTEA NOT NULL
  , name BYTEA NOT NULL
  , CONSTRAINT pk_asset PRIMARY KEY (id)
  , CONSTRAINT unique_asset UNIQUE (policyId, name)
  );

INSERT INTO chain.asset (policyId, name)
  ( SELECT DISTINCT policyId, name
    FROM chain.assetOut
    UNION
    SELECT DISTINCT policyId, name
    FROM chain.assetMint
  );

ALTER TABLE chain.assetOut
  ADD COLUMN assetId INT;

ALTER TABLE chain.assetMint
  ADD COLUMN assetId INT;

UPDATE chain.assetOut
SET
  assetId = asset.id
FROM chain.asset
WHERE assetOut.policyId = asset.policyId
  AND assetOut.name = asset.name;

UPDATE chain.assetMint
SET
  assetId = asset.id
FROM chain.asset
WHERE assetMint.policyId = asset.policyId
  AND assetMint.name = asset.name;

ALTER TABLE chain.assetOut
  ADD CONSTRAINT fk_assetOut_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id),
  ALTER COLUMN assetId SET NOT NULL,
  DROP COLUMN policyId,
  DROP COLUMN name;

ALTER TABLE chain.assetMint
  ADD CONSTRAINT fk_assetMint_asset FOREIGN KEY(assetId) REFERENCES chain.asset(id),
  ALTER COLUMN assetId SET NOT NULL,
  DROP COLUMN policyId,
  DROP COLUMN name;

CREATE INDEX assetMint_assetId ON chain.assetMint USING BTREE (assetId);
CREATE INDEX assetOut_assetId ON chain.assetOut USING BTREE (assetId);

COMMIT;
