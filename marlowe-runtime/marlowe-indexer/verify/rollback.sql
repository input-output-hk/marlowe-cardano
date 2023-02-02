-- Verify marlowe:rollback on pg

BEGIN;

SELECT fromBlock, toBlock, toSlotNo FROM marlowe.rollbackBlock;

ROLLBACK;
