
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
