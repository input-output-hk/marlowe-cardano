
<a id='changelog-0.0.5'></a>
# 0.0.5 — 2023-09-22

## Added

- Assets to several response schemas
  - In `Payout` - shows the assets in the tx output corresponding to the payout.
  - In `ContractState` - shows the assets in the current UTxO of the contract (the account balances). Empty if the contract has no UTxO (i.e. the contract is closed).
  - In `Tx` - shows the assets in the marlowe script output of that transaction (the account balances). Empty if the transaction does not produce an output.
- Payouts produced by a transaction are added to `Tx`

## Changed

- Custom error formatter for request body parsing failures
- More details displayed about internal server errors

- Tokens json objects are deserialized now without the `unTokens` field.

- Use of generics for REST error reporting

- Improved quality of generated Open API documentation

## Fixed

- Serialization for Token Names containing "."

<a id='changelog-0.0.4'></a>
# 0.0.4 — 2023-08-10

## Added

* New Endpoint `GET` : `contracts/:contractId/next` (Provide a description of what can be done for a given contract)

- Results of contract's safety analysis reported in create response.

* Unclaimed payouts added in `/contracts/:contractId`

* Optional Filetring by Party for `GET` : `contracts/:contractId/next`

- Added header `X-Stake-Address` for creating contracts.

<a id='changelog-0.0.2'></a>
# 0.0.2 — 2023-06-15

## Added

* These endpoints `POST` handlers were extended so they return transaction as a result (instead of transaction body) if a particular header is present:

  * `contracts` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.contract-tx-json" header

  * `transactions` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.apply-inputs-tx-json" header

  * `withdraw` endpoint uses "Accept: application/vendor.iog.marlowe-runtime.withdraw-tx-json" header

* All the above endpoints accept also `witnessset` as a payload for `PUT` request now.
