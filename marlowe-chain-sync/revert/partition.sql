-- Revert chain:partition from pg

BEGIN;

DROP FUNCTION chain.createSlotPartition;

COMMIT;
