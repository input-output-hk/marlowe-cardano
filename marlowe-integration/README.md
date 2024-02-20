# Marlowe Integration

A library that supports spinning up Marlowe Runtime instances in an ephemeral
testnet for integration testing.

## Running

In order to run the test suite you can invoke `cabal` directly. You can also pass a filter to the command to select a subset of the test suite:

```
$ cabal run marlowe-integration-tests -- --match "POST /contracts safety report"
```

## Prerequisites

The library functions use `exec` to spawn external processes, and therefore the
following programs need to be in the `PATH` when you use them:

- `cardano-cli`
- `cardano-node`
- `sqitch`

Additionally, you are required to have a PostgreSQL server running and
available.

### Setting local user space PostgreSQL

It is possible to setup for testing a fully local postresql cluster - it is pretty lightweight solution.

``` shell
$ mkdir .pgdata # PLEASE READ MORE DETAILS ABOUT THE PATH LENGTH
$ export PGDATA=.pgdata
$ pg_ctl init
```

If your current working directory has long absolute path PostgreSQL can actually fail to run (it has sever limit on the length of the path).
To fix this you can link just created directory to some shorter path like:

```
$ ln -s $PWD/.pgdata /home/user/.pgdata
$ export PGDATA=/home/user/.pgdata
```

In order it to work we should change few things in the `.pgdata/postgresql.conf`:

* `unix_socket_directories = '.'`

* `port = SOME_NON_STANDARD_PORT_LIKE_15432`

Then to use this setup during testing you should setup db user so it is the same as your current system user:

```shell
export MARLOWE_RT_TEST_DB_USER=myuser
export MARLOWE_RT_TEST_DB_PORT=USE_THE_SAME_PORT_AS_ABOVE
```

### Setting up a PostgreSQL database (NixOS)

To set up a PostgreSQL database in NixOS, you can use the `services.postgresql` option in your NixOS configuration. Here's an example:

```nix
services.postgresql = {
  enable = true;
  package = pkgs.postgresql;
  enableTCPIP = true;
  authentication = pkgs.lib.mkOverride 10 ''
    local all all trust
    host all all ::1/128 trust
    host all all 127.0.0.1/32 trust
  '';
};
```

This configuration enables the PostgreSQL service, installs the `postgresql` package, and allows connections from localhost (`127.0.0.1`) using the default user `postgres` which starts with no password. The `authentication` option adds entries to the `pg_hba.conf` file to allow the integration tests to connect locally.

### Customizing the PostgreSQL configuration

You can configure the connection settings by exporting the following environment variables using the `export` command:
- `MARLOWE_RT_TEST_DB_HOST` sets the host of the postgresql server (by default: `127.0.0.1`).
- `MARLOWE_RT_TEST_DB_PORT` sets the port of the postgresql server (by default: `5432`).
- `MARLOWE_RT_TEST_DB_USER` sets the user that will be used to connect to the postgresql server as part of the tests (by default: `postgresql`). User must have rights to create databases (`CREATEDB`).
- `MARLOWE_RT_TEST_DB_PASSWORD` sets the password that will be used to authenticate the connection to postgresql server as part of the tests (by default no password or the empty string).
- `MARLOWE_RT_TEST_TEMP_DB` sets the template database to be used when creating test databases (by default `template1`), it needs to exist and be accessible by the user specified.
- `MARLOWE_RT_TEST_DB` sets the test database name so you can easily connect to it and inspect the data. By default the database name is random.
- `MARLOWE_RT_TEST_CLEANUP_DATABASE` determines whether to delete the test databases after the tests (by default `true`).

### Inspecting database during test execution

Sometimes when a particular test case is failing it can be beneficial to suspend the execution, connect to the test database and inspect the data. In order to this we have to

* put some suspension into your test case: `threadDelay (5 * 60 * 1000_000)`

* define test database name: `$ export MARLOWE_RT_TEST_DB_NAME=runtime-integration`

* run the test case

* then you can jump into to db shell: `$ psql -U myuser --host=$PGDATA --port=THE_SAME_PORT_AS_ABOVE runtime-integration`

* in order to inspect marlowe indexer data you **have to** set appropriate schema: `psql> SET search_path TO marlowe;`

* in order to inspect chain indexer data you **have to** set appropriate schema: `psql> SET search_path TO chain;`


## Usage

The functions `withLocalMarloweRuntime` and `withLocalMarloweRuntime'` allow
you to run an action in the context of a running Marlowe runtime and Cardano
testnet. `withLocalMarloweRuntime'` allows configuration to be explicitly
provided, `withLocalMarloweRuntime` uses default configuration.

Here is the general idea:

```hs
myTest = withLocalMarloweRuntime \MarloweRuntime{..} -> do
  -- Test code here
```

the `MarloweRuntime` record contains:

- Client runners for all public-facing Marlowe runtime APIs
- The `LocalTestnet` record for the Cardano network.

## Notes on performance

You probably want to share an instance if running a test suite, as it takes a
considerable amount of time to start the cardano testnet. You can use hooks
like Hspec's `aroundAll` to achieve this:

```hs
myTestSuite = aroundAll withLocalMarloweRuntime do
  describe "context 1" do
    it "test 1" \MarloweRuntime{..} -> _ -- test code here
    it "test 2" \MarloweRuntime{..} -> _ -- test code here
  describe "context 2" do
    it "test 3" \MarloweRuntime{..} -> _ -- test code here
    it "test 4" \MarloweRuntime{..} -> _ -- test code here
```

