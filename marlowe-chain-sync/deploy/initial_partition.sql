-- Deploy chain:initial_partition to pg
-- requires: partition
-- requires: block
-- requires: tx

BEGIN;

CREATE FUNCTION chain.createInitialPartitions(name text)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  FOR n IN 0..10
  LOOP
    PERFORM chain.createSlotPartition(name, n);
  END LOOP;
END;
$$;

SELECT chain.createInitialPartitions('block');
SELECT chain.createInitialPartitions('tx');
SELECT chain.createInitialPartitions('txOut');
SELECT chain.createInitialPartitions('txIn');

DROP FUNCTION chain.createInitialPartitions;

COMMIT;
