-- Deploy chain:partition to pg
-- requires: appschema

BEGIN;

CREATE FUNCTION chain.createSlotPartition(name text, n integer)
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  suffix text;
  lowerBound bigint;
  upperBound bigint;
BEGIN
  suffix :=
    CASE
      WHEN n = 0 THEN '0'
      ELSE format('%sm', n)
    END;
  lowerBound :=
    CASE
      WHEN n = 0 THEN -1
      ELSE n * 10000000
    END;
  upperBound := (n + 1) * 10000000;
  EXECUTE format
    ( 'CREATE TABLE chain.%s_%s PARTITION OF chain.%s FOR VALUES FROM (%s) TO (%s);'
    , name
    , suffix
    , name
    , lowerBound
    , upperBound
    );
END;
$$;

COMMIT;
