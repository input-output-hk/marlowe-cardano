-- Verify chain:tx on pg

BEGIN;

SELECT id, blockId, hash, validityLowerBound, validityUpperBound, metadataKey1564
  FROM chain.tx
 WHERE FALSE;

SELECT id, txId, ix, address, lovelace, datumHash, datumBytes
  FROM chain.txOut
 WHERE FALSE;

SELECT id, txId, txOutId, redeemerDatumBytes
  FROM chain.txIn
 WHERE FALSE;

ROLLBACK;
