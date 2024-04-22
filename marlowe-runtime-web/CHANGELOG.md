
<a id='changelog-1.0.0'></a>
# 1.0.0 — 2024-04-22

## Added

- Enhanced error information (including CoinsSelectionError, MarloweTransactionError, etc.).

- Optional Initialization of Participant's Account during contract creation
  - Allows simultaneous deposits during the creation process

- Safety Errors are available at :
  - Contract creation (Building the Tx) : `POST /contracts`
  - Input application (Building the Tx) : `POST /contracts/:contractId/transactions`

- Additional semantics info to response from `GET /contracts/:contractId/transactions/:transactionId`
  - `inputState` is the Marlowe state from the transaction's input datum.
  - `inputContract` is the Marlowe contract from the transaction's input datum.
  - `reconstructedSemanticInput` is the reconstructed `TransactionInput` that
    was passed to `computeTransaction` during the execution of the Marlowe
    semantics validator. This includes the correct `TimeInterval` against which
    to evaluate the input contract and state. The upper bound of this interval
    differs from `invalidHereafter` by -1 milliseconds.
  - `reconstructedSemanticsOutput` is the reconstructed `TransactionOutput`
    that calling `computeTransaction` inside the validator produces. This can
    be used to recover any intra-state payment information, as well as anyO
    warnings which were raised during evaluation. This information is not
    otherwise available in the response.

## Changed

- Transitioned REST API error encoding from a generic format to a more explicit error structure.

<a id='changelog-0.0.6'></a>
# 0.0.6 — 2023-12-06

## Added

- Added support for creating and applying input to "open role" contracts.

## Changed

- Fixed errors and added clarification to Open API documentation.

## Fixed

- 5xx responses do not include CORS headers.

<a id='changelog-0.0.5.1'></a>
# 0.0.5.1 — 2023-10-17

## Fixed

- `/next` endpoint broken with added `/getNextStepsForContract` path segment.

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
