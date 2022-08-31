-- Verify chain:asset on pg

BEGIN;

SELECT id, policyId, name
  FROM chain.asset
 WHERE FALSE;

SELECT txOutId, txOutIx, slotNo, assetId, quantity
  FROM chain.assetOut
 WHERE FALSE;

SELECT txId, slotNo, assetId, quantity
  FROM chain.assetMint
 WHERE FALSE;

ROLLBACK;
