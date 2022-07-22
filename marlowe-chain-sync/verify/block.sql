-- Verify chain:block on pg

BEGIN;

SELECT hash, slotNo, blockNo, rollbackToBlock, rollbackToGenesis
  FROM chain.block
 WHERE FALSE;

ROLLBACK;
