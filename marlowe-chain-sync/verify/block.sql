-- Verify chain:block on pg

BEGIN;

SELECT id, slotNo, blockNo, rollbackToBlock, rollbackToSlot
  FROM chain.block
 WHERE FALSE;

ROLLBACK;
