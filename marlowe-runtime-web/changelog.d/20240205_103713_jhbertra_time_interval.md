### Added

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
    be used to recover any intra-state payment information, as well as any
    warnings which were raised during evaluation. This information is not
    otherwise available in the response.
