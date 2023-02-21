# Marlowe Integration

A library that supports spinning up Marlowe Runtime instances in an ephemeral
testnet for integration testing.

## Prerequisites

The library functions use `exec` to spawn external processes, and therefore the
following programs need to be in the `PATH` when you use them:

- `cardano-cli`
- `cardano-node`
- `sqitch`

Additionally, you are required to have a PostgreSQL server running and
available.

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

