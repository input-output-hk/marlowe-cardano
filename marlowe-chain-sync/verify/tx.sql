-- Verify chain:tx on pg

BEGIN;

SELECT id, blockHash, validityLowerBound, validityUpperBound, metadataKey1564
  FROM chain.tx
 WHERE FALSE;

SELECT txId, txIx, address, lovelace, datumHash, datumBytes
  FROM chain.txOut
 WHERE FALSE;

SELECT txOutId, txOutIx, txInId, redeemerDatumBytes
  FROM chain.txIn
 WHERE FALSE;

ROLLBACK;
