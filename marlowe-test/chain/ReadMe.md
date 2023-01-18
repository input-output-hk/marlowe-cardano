# Comparison of Marlowe Chain Index to Cardano DB Sync

The `bash` script [`compare.sh`](compare.sh) uses the SQL script [`compare.sql`](compare.sql) to peform a record-by-record comparison of the contents of a Marlowe chain index database against a Cardano DB Sync database, place the results in the folder `out/`. If needed, set the user, database, host, and port using `PGUSER`, `PGDATABASE`, `PGHOST`, and `PGPORT`.

This script assumes that the `marlowe-chainindexer` tables reside in the schema `chain` and the `cardano-db-sync` tables reside in the schema `public`. Two strategies for moving a `cardano-db-sync` database to a `public` schema from a different database are . . .

1. Use `pg_dump` and `psql` to copy the data.
2. Use [dblink](https://www.postgresql.org/docs/current/contrib-dblink-function.html) in a `create view` for each table.
