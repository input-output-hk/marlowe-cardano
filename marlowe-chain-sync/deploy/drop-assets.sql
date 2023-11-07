-- Deploy chain:drop-assets to pg
-- requires: asset

BEGIN;

ALTER TABLE chain.assetOut
  ADD COLUMN policyId BYTEA,
  ADD COLUMN name BYTEA;

ALTER TABLE chain.assetMint
  ADD COLUMN policyId BYTEA,
  ADD COLUMN name BYTEA;

UPDATE chain.assetOut
SET
  policyId = asset.policyId,
  name = asset.name
FROM chain.asset
WHERE asset.id = assetId;

UPDATE chain.assetMint
SET
  policyId = asset.policyId,
  name = asset.name
FROM chain.asset
WHERE asset.id = assetId;

DROP INDEX chain.assetOut_assetId;
DROP INDEX chain.assetMint_assetId;

ALTER TABLE chain.assetOut
  DROP CONSTRAINT fk_assetOut_asset,
  ALTER COLUMN policyId SET NOT NULL,
  ALTER COLUMN name SET NOT NULL,
  DROP COLUMN assetId;

ALTER TABLE chain.assetMint
  DROP CONSTRAINT fk_assetMint_asset,
  ALTER COLUMN policyId SET NOT NULL,
  ALTER COLUMN name SET NOT NULL,
  DROP COLUMN assetId;

DROP TABLE chain.asset;

COMMIT;
