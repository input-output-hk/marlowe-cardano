-- Verify chain:block on pg

BEGIN;

SELECT id, hash, slotNo, blockNo, rollbackBlock
  FROM chain.block
 WHERE FALSE;

ROLLBACK;
