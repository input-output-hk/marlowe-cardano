-- Verify chain:tx on pg

BEGIN;

SELECT id, blockId, slotNo, validityLowerBound, validityUpperBound, metadataKey1564, isValid
  FROM chain.tx
 WHERE FALSE;

SELECT txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral
  FROM chain.txOut
 WHERE FALSE;

SELECT txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral
  FROM chain.txIn
 WHERE FALSE;

ROLLBACK;
