-- Verify chain:asset on pg

BEGIN;

SELECT id, policyId, name
  FROM chain.asset
 WHERE FALSE;

SELECT id, txOutId, assetId, quantity
  FROM chain.assetOut
 WHERE FALSE;

SELECT id, txId, assetId, quantity
  FROM chain.assetMint
 WHERE FALSE;

ROLLBACK;
