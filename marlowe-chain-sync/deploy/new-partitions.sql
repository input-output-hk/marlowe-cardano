-- Deploy chain:new-partitions to pg
-- requires: initial_partition

BEGIN;

CREATE FUNCTION pg_temp.createPartitions200m(name text)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  FOR n IN 11..20
  LOOP
    PERFORM chain.createSlotPartition(name, n);
  END LOOP;
END;
$$;

SELECT pg_temp.createPartitions200m('block');
SELECT pg_temp.createPartitions200m('tx');
SELECT pg_temp.createPartitions200m('txOut');
SELECT pg_temp.createPartitions200m('txIn');

COMMIT;
