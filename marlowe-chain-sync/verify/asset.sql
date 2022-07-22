-- Verify chain:asset on pg

BEGIN;

SELECT id, policyId, name
  FROM chain.asset
 WHERE FALSE;

SELECT txId, txIx, assetId, quantity
  FROM chain.assetOut
 WHERE FALSE;

SELECT txId, assetId, quantity
  FROM chain.assetMint
 WHERE FALSE;

ROLLBACK;
