-- Deploy chain:initial_partition to pg
-- requires: partition
-- requires: block
-- requires: tx

BEGIN;

CREATE FUNCTION pg_temp.createInitialPartitions(name text)
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

SELECT pg_temp.createInitialPartitions('block');
SELECT pg_temp.createInitialPartitions('tx');
SELECT pg_temp.createInitialPartitions('txOut');
SELECT pg_temp.createInitialPartitions('txIn');

COMMIT;
