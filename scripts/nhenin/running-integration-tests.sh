#!/usr/bin/env bash
# export MARLOWE_RT_TEST_DB_HOST=127.0.0.1
export PGDATA=.pgdata
export MARLOWE_RT_TEST_DB_PORT=15432
export MARLOWE_RT_TEST_DB_USER=parallels
export MARLOWE_RT_TEST_DB=marlowe-integration
# export MARLOWE_RT_TEST_CLEANUP_DATABASE="true"
# export MARLOWE_RT_TEST_DB_PASSWORD=
# export MARLOWE_RT_TEST_TEMP_DB=

# psql -U $MARLOWE_RT_TEST_DB_USER --host=$PGDATA --port=$MARLOWE_RT_TEST_DB_PORT $MARLOWE_RT_TEST_DB
# dropdb marlowe-integration -U parallels --port 15432 --host /home/parallels/dev/marlowe-lang/.pgdata; cabal run marlowe-integration-tests -- --match "e2e" --fail-fast