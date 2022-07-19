-- Revert chain:asset from pg

BEGIN;

DROP TABLE chain.assetMint;
DROP TABLE chain.assetOut;
DROP TABLE chain.asset;

COMMIT;
