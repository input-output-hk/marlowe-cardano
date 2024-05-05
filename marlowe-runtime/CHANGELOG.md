
<a id='changelog-1.0.0'></a>
# 1.0.0 — 2024-04-22

## Removed

- `marlowe-runtime-cli`

## Added

- Support for initial account balances when creating contracts

- Upgrade to `cardano-api` 8.37.1.1

- A safety analysis to the apply input step

- Support for compiling/building/running the repository using new Mac CPUs (M1/M2/M3) within a linux VM (`aarch64-linux` system).

## Changed

- Updated Marlowe Plutus scripts to use optmized `asData` for `Case`.

- Default batch size for chain-indexer decreased to 1/4 to prevent OOM errors.

- Upgrade `cardano-api` `8.37.1.1` to `8.39.2.0` (Conway Hard Fork Adaptation)
- Upgrade `plutus` from `1.15` to `1.21` (`plutus-core`, `plutus-ledger-api`, `plutus-tx`) (Conway Hard Fork Adaptation)
- Upgrade `cardano-ledger` (Conway Hard Fork Adaptation)
    - `cardano-ledger-conway` from `1.11` to `1.12`
    - `cardano-ledger-core` from `1.9` to `1.10`
    - `cardano-ledger-mary` from `1.4` to `1.5`
    - `cardano-ledger-shelley` from `1.8` to `1.9`
    -  `cardano-ledger-binary` from `1.2` to `1.3`
- Upgrade `ouroboros-network` (Conway Hard Fork Adaptation)
    - `ouroboros-network-api` from `0.6.0` to `0.7.0`
    - `ouroboros-consensus` from `0.14` to `0.16`
    - `ouroboros-network-protocols` from `0.6.0` to `0.8`

## Fixed

- Marlowe Indexer can crash when chain indexer is syncing from Genesis.

- `marlowe-contract` consumes a lot of memory.
  - The garbage collector was holding onto a rolling window of snapshots
    spanning 2160 blocks of all live Marlowe contracts. It only ever needed to
    hold onto the datum hashes of the merkleized continuations in the
    contracts, so rather than keeping all the contracts around it now just
    holds onto these.

- Space leak in `marlowe-contract`

- Duplicate blocks can break marlowe-chain-sync queries
- Delete all blocks when rolling back to Genesis.

- Arbitrary Marlowe instances which were not generating invalid currencies and tokens.

- Safety analysis transaction filtering which was causing selective tx reporting.

<a id='changelog-0.0.6'></a>
# 0.0.6 — 2023-12-06

## Added

- New bulk sync protocol which allows clients to subscribe to all events from
  all marlowe contracts.

- Garbage collection added to `marlowe-contract`

- More documentation in --help text

- Added support for creating and applying input to "open role" contracts.

- Safety analysis support for open-role contracts.

- `marlowe-chain-copy` executable for efficiently seeding a new chain database.

- Support for advanced role token distribution.
  - Mint multiple tokens per role
  - Send tokens to multiple recipients.

- Arbitrary extra properties can be added to role token metadata.

- Published Marlowe validators compiled under plutus-1.15.0.0.

## Changed

- Min lovelace deposit is optional, and a default will be computed if omitted.

- Improve signal-to-noise ratio of chain sync protocol logging.

- Dropped `asset` table from `chain` schema, inlining fields into `assetOut`
  and `assetMint`.
- Improved sync performance of `marlowe-chain-indexer` by switching from
  `INSERT` to `COPY`.
- Internal: consolidated database row extraction logic for cardano blocks into
  reusable modules.

- Improved sync performance of `marlowe-indexer`.

- `marlowe-indexer` now indexes contracts by parties in the accounts as well as
  the contract.

## Fixed

- Building a transaction for a contract that uses ADA role tokens fails.

- The `TransactionInput` definition in the generated OpenApi schema for Marlowe Runtime Web is now explicitly typed as an `object`.

- Safety checks fail when open roles and default thread token name are used.

- The generated OpenApi schema represented tuples as heterogenous arrays which was not allowed with OpenApi specification 3.0.0. The schema was set to target OpenApi specification 3.1.0.

<a id='changelog-0.0.5'></a>
# 0.0.5 — 2023-09-22

## Added

- New payouts API that supports:
  - Querying payouts with filtering support for contract IDs, role tokens, and
    claim status.
  - Querying a single payout by its ID

- New filters to `GetHeaders` query to allow filtering contract headers by party
  (addresses and role tokens). Specifying parties in the filter will limit
  results to contracts which (visibly) contain either the address or role token as a
  party.
  - Regarding "visibly contain": this applies to merkleized contract. A
    merkleized contract visibly contains a party if the party can be extracted
    from its on-chain history without having access to the continuations. This
    means any parties contained in unexecuted paths of the contract are not
    included, because they aren't visible.
- New query parameters for `/contracts` endpoint `partyAddress` and `partyRole`.
  These give REST API access to the party filtering functionality mentioned
  above.

- Added new validator scripts to script registry.

- GHC 9.2 support

## Changed

- BREAKING: marlowe-tx: Create, ApplyInputs, and Withdraw now use the era reported by the node to build the TxBody.
  - Era is queried from the node at the time of job execution.
  - The era information is returned in the results of the jobs.
  - If the era does not support reference scripts (i.e. prior to Babbage) the
    command will fail wit an `EraUnsupported` error.
- BREAKING: marlowe-tx: Submit now accepts transactions from any era that
  supports reference scripts, not exclusively babbage. This is a breaking
  change because the era information must now be included in the command.

- BREAKING `Withdraw` now accepts a set of payout tx outs instead of a contract
  ID and a role token. The old behaviour can be emulated via a query to fetch
  unclaimed payouts for a contract.

- Updated script hashes in script-registry test.

- BREAKING: cardano-api, plutus, cardano-ledger dependencies track with cardano-api 8.2
- BREAKING: `marlowe-tx` and `marlowe-runtime` now require a command line option for a program to generate the role token minting validator.
- BREAKING: Requires cardano-node 8.1.2

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
