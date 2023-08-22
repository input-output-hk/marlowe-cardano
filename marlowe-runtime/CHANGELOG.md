
<a id='changelog-0.0.4'></a>
# 0.0.4 — 2023-08-10

## Removed

- BREAKING `MarloweTracedT` - now there is only `MarloweT`.

- Custom Marlowe JSON with imports format for `marlowe-runtime-cli load`

- Traces from chain sync and chain seek clients

## Added

- `marlowe-runtime` executable and Docker image providing an all-in-one runtime option.

- Safety analysis integrated into contract creation result.

- New `ReqStatus` query to Marlowe Query that returns sync info and version info.
- Status information is now included in response headers of all REST API calls.

- Added `--stake-address` to `marlowe-runtime-cli create` command, for attaching stake credentials to Marlowe's validator address.

- New main protocol `MarloweTransfer` for copying contracts to / from contract store.
- `marlowe-runtime-cli export` command to download a contract bundle from the store.

- POST /contracts/sources endpoint provides access to contract import in the
  REST API (via MarloweTransfer).

- `/contracts/sources/:contractSourceId` endpoint with `GET` method and two sub-resources for `adjacency` and `closure`.

- Request body for `POST /contracts` now accepts a contract source ID as well as contract JSON.

- Console logs for marlowe-chain-indexer and marlowe-indexer sync progress

## Changed

- `marlowe-runtime-cli load` now accepts a contract bundle

- Sync performance of `marlowe-indexer` improved via bulk queries.

- New partitions added to accommodate slots upt to 200 million in chain database.

- BREAKING `MerkleizeInputs` accepts a contract instead of a hash.

- Signature of `Language.Marlowe.Runtime.Transaction.Safety.checkTransactions` change to use protocol parameters in place of constraint solver.

## Fixed

- Metadata tag length cannot be longer than 64 characters
  - metadata tags now get chunked into lists of text of max. 64 characters
    each to comply with the CDDL specification for transaction metadata.

- Importing a JSON contract doesn't work
- Importing an exported contract archive doesn't work

- Computation of maximum possible Plutus execution costs now rounds upwards.

- Transaction submission hangs when chain sync loses connection to node.

- Auto-merkleization of inputs fails if contract in datum got reduced.

- Removed inconsistency in system start for safety checks (PLT-6849).

<a id='changelog-0.0.3'></a>
# 0.0.3 — 2023-07-11

## Fixed

- Added migration to change tags column from varchar(64) to text.

<a id='changelog-0.0.2'></a>
# 0.0.2 — 2023-06-15

## Removed

- JSON console logs
- BREAKING - `--log-config-file` command line options
- BREAKING - `--print-log-config` commands

## Added

- Safety checks for Marlowe contracts.

- OpenTelemetry-based tracing
- Second port to `marlowe-proxy` for clients that provide their own tracing
  information.

- `marlowe-contract` service for contract definition storage.
    - Includes a file-based store and a server for the `MarloweLoad` protocol.
- BREAKING `MarloweLoad` as a sub-protocol of `MarloweRuntime` service
    - `marlowe-proxy` now requires a host and port for `marlowe-contract`
- functions for running `MarloweLoad` clients to `marlowe-client`
- `load` command to `marlowe-runtime-cli`

- Contract query to `marlowe-proxy`
- `query store` commands to `marlowe-runtime-cli`

- `MarkleizeInputs` request to `ContractRequest` (`marlowe-contract` query API)

- Option to pass a `DatumHash` to `marlowe-tx`'s `Create` command. This will
  load the matching contract from the contract store.
- `--contract-hash` option to `marlowe-runtime-cli create` to pass a contract hash
  obtained via `marlowe-runtime-cli load` when creating a contract.

- Safety checks for invalid Plutus addresses.

- Uncaught exceptions in server threads are now logged to stderr rather than
  being swallowed.

## Changed

- [Internal] tracing code refactored to use a mtl-style class instead of
  explicit tracer passing.

- BREAKING - Extracted generic query protocol and reworked `marlowe-sync`'s
  query protocol to use it.

- `Apply` will auto-merkleize normal inputs to a contract if the contract is in
  the store.
  - E.g. Given an on-chain contract `When [MerkleizedCase (Notify TrueObs) "foo"] 0 Close`,
    a client can call `Apply` with `[NormalInput INotify]` and `marlowe-tx` will
    merkleize it automatically if it can find the contract in the store.
    Clients can still pass manually merkleized inputs to `Apply` if desired.

- Console log formatting.
- Increased the maximum confirmation wait time in `marlowe-tx` to 1 hour.

- If `OTEL_EXPORTER_OTLP_ENDPOINT` env var is not set, runtime will not attempt
    to send traces anywhere.

## Fixed

- Implements missing support in the Marlowe Runtime CLI `apply` command to accept input files through the `--input-file` argument.

- Driver state threading in runPeerDriverTraced

- `marlowe-runtime-cli choose --help` displayed description for `notify`.

- Console logs are now thread-safe.
- `marlowe-scaling` did not delay polling at all when submitting a transaction.
