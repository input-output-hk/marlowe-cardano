
<a id='changelog-1.0.0'></a>
# 1.0.0 — 2024-04-22

## Removed

- `watch` command
- Alonzo era support

## Added

- Upgrade to `cardano-api` 8.37.1.1

## Changed

- Marlowe CLI swap template tokens default to ada if not specified.

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

- Marlowe CLI withdrawal no longer includes a validity interval.

- `analyze` command so it doesn't override provided state.

<a id='changelog-0.2.0.0'></a>
# 0.2.0.0 — 2023-12-06

## Fixed

- Fixed `marlowe-cli` flags `--submit` and `--timeout` so they are consistent and so that the parser accepts an integer with or without the `s` suffix.

- The `marlowe-cli transaction find-published` now reports correct information on the open-role validator.

<a id='changelog-0.1.0.0'></a>
# 0.1.0.0 — 2023-09-22

## Removed

- BREAKING inoperative chain sync connection options from `test` command removed.

## Added

- Format conversion with `marlowe-cli format`

- Open role Plutus script which release role token on deposit.

- Simulation mode to the testing DSL which maintains in memory UTxO set.

- Extra safety analytical cases for accompaniaing scripts.

## Changed

- Print list of commands when marlowe-cli is invoked without a command

- Pre-compiled scripts are used for all commands using validators.

- BREAKING: Requires cardano-node 8.1.2

<a id='changelog-0.0.12.0'></a>
# 0.0.12.0 — 2023-08-11

## Added

- Support for client side merkleization in both Runtime and CLI based runners.

- Support for externaly minted currencies through currency symbol in the DSL.

- Support for loading external wallets.

- Support for direct usage of external addresses in the minting process and in contracts.

- Golden test for Plutus execution-cost analysis (PLT-6907).

## Changed

- Minting logic so it creates minimal set of token bundled UTxOs

<a id='changelog-0.0.11.0'></a>
# 0.0.11.0 — 2023-06-15

## Removed

- Removed `RuntimeAwaitInputsApplied`, `RuntimeAwaitContractCreated` - the can be easily simulated by using the new `awaitConfirmed` param.

## Added

- Test runner for the Runtime with basic set of operations (we still miss `withdraw`).

- Implementation for all the basic (roless) flows with template contracts from marlowe-contracts.

- Improved ergonimcs of the "DSL" (simpler structure, nicer field names etc.).

- Improved balance checking.

- Added `awaitConfirmed` - most of the actions provide this optional await setup now.

- Added `RuntimeWithdraw` action to the marlowe-cli testing DSL.

- Added `RuntimeAwaitTxsConfirmed` action which awaits all the pending Marlowe transaction till they are registered in the Runtime.

- Ports of old inline role-based tests `./marlowe-cli/tests/inline/*` (currently only marlowe-cli based flows)

- `--max-retries` option to the test command which performs retries based on the type of the failure

- More `yaml` DSL rewrites of operation properties (handling more compact representations) and Marlowe contract (handling relative timeouts)

- `ShouldFail` operation

## Changed

- Changed encoding of operations so we don't use `tag` field but instead a constructor name is a key in a singleton object.

- Improve report structure to incorporate more details.

- Error handling across the interpreters by introducing more semantic error representation

## Fixed

  - Funds management. Bracketing is applied to every subfaucet or funds acquisition.

- Value parsing of the `--submit` option in all the relevant commands.
