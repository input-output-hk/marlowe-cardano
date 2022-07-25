-- Revert chain:partition from pg

BEGIN;

DROP FUNCTION createSlotPartition;

COMMIT;
