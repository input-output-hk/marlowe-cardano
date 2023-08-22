
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
