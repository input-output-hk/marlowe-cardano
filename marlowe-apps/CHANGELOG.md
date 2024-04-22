
<a id='changelog-1.0.0'></a>
# 1.0.0 — 2024-04-22

## Removed

- Removed `marlowe-scaling` executable because `marlowe-benchmark` now serves this function.

- Removed `marlowe-finder` executable because `marlowe-benchmark` now serves this function.

- Removed `marlowe-streamer` executable because `marlowe-benchmark` now serves this function.

- Removed `marlower-signer` executable.

## Changed

- Upgrade to cardano-api 8.37.1.1

- Upgrade `cardano-api` `8.37.1.1` to `8.39.2.0` (Conway Hard Fork Adaptation)
- Upgrade `plutus` from `1.15` to `1.21` (`plutus-core`, `plutus-ledger-api`, `plutus-tx`) (Conway Hard Fork Adaptation)

<a id='changelog-0.4.0.0'></a>
# 0.4.0.0 — 2023-12-06

## Changed

- Fixed `marlowe-apps` and `marlowe-finder` to handle new Marlowe chain sync protocol semantics where payout redemption is present in the event stream.

- [BREAKING] Output of marlowe-finder has changed. It now outputs JSON objects with the following event types:
  - `"NewBlock"` when a new block in the chain is encountered.
  - `"NewContract"` when a new contract is found.
  - `"InputsApplied"` when inputs are applied to a contract.
  - `"PayoutsWithdrawn"` when role payouts are withdrawn from a payout validator.
- Performance of marlowe-finder improved significantly.

<a id='changelog-0.3.0.0'></a>
# 0.3.0.0 — 2023-09-22

## Removed

- BREAKING inoperative chain sync connection options removed from all executables.

## Added

- Executable `marlowe-streamer` for server-sent event (SSE) stream of Marlowe contracts.

- Added a random-number oracle.
- Added an external-process oracle.

- Docker images for executables.

- Added `marlowe-streamer` client that streams Marlowe contracts using HTTP server-side events (SSE).

<a id='changelog-0.2.5.0'></a>
# 0.2.5.0 — 2023-06-15

## Removed

- Removed `MarloweRequest(Wait)` from `Language.Marlowe.Runtime.App.Types`.

## Deprecated

- Deprecated `submit'` and `waitForTx'` in `Language.Marlowe.Runtime.App.Submit`.

## Fixed

- `exe:marlowe-scaling` now successfully submits transactions.

## Added

- Added simple transaction signing service for use in demonstrations.
