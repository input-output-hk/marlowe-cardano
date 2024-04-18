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

### Setting up User-Space PostgreSQL

It is possible to set up a PostgreSQL cluster at the user level without needing to adjust system-level configuration or require superuser privileges. To set up a local database cluster, you can use the following commands:

``` shell
$ mkdir .pgdata
$ export PGDATA=.pgdata
$ pg_ctl init
```

If your current working directory has a long absolute path, PostgreSQL may fail to run due to its severe limitation on the length of the path. To fix this, you can link the newly created directory to a shorter path, like:

```
$ ln -s $PWD/.pgdata /home/myuser/.marlowe-integration-pgdata
$ export PGDATA=/home/myuser/.marlowe-integration-pgdata
```

Additinally we have to adjust two postgresql options in `.pgdata/postgresql.conf` to enforce custom port (so you don't experience conflicts with your system postgresql instance) and to allow local connections:

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

## Debugging

### Inspecting Database During Test Execution

Sometimes, when a particular test case is failing, it can be beneficial to suspend the execution, connect to the test database, and inspect the data. To do this, follow these steps:

1. Insert a suspension point in your test case: `threadDelay (5 * 60 * 1000_000)`.
2. Define the test database name: `export MARLOWE_RT_TEST_DB_NAME=runtime-integration`.
3. Run the test case.
4. Then, connect to the database shell using: `psql -U myuser --host=$PGDATA --port=THE_SAME_PORT_AS_ABOVE runtime-integration`.
5. To inspect Marlowe indexer data, set the appropriate schema with: `psql> SET search_path TO marlowe;`.
6. To inspect chain indexer data, set the appropriate schema with: `psql> SET search_path TO chain;`.

### Logs and Traces

During test execution, Marlowe Runtime traces and logs are written to text files under the "workspace" directory. Workspaces are created as subdirectories of `/tmp/workspaces`. The name of each workspace is generated randomly. To simplify debugging, consider cleaning up this directory before running a specific single scenario. Please note that workspaces are **cleaned up** after a successful run, so logs from failed test runs are the ones available.

To enable Runtime logging to `stderr`, use the following environment variable: `export MARLOWE_RT_LOG_STDERR=True`. The same information is also written to the log file in the workspace directory.

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

