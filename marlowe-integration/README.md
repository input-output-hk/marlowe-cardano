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
available. You can configure the connection settings with the following
environment variables:

- `MARLOWE_RT_TEST_DB_HOST` (default: `127.0.0.1`)
- `MARLOWE_RT_TEST_DB_PORT` (default: `5432`)
- `MARLOWE_RT_TEST_DB_USER` (default: `postgres`)
- `MARLOWE_RT_TEST_DB_PASSWORD` (default `<blank>`)
- `MARLOWE_RT_TEST_TEMP_DB` (default: `template1`)

## Usage

the functions `withLocalMarloweRuntime` and `withLocalMarloweRuntime'` allow
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

## Notes on performance:

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
