### Changed

- [BREAKING] Output of marlowe-finder has changed. It now outputs JSON objects with the following event types:
  - `"NewBlock"` when a new block in the chain is encountered.
  - `"NewContract"` when a new contract is found.
  - `"InputsApplied"` when inputs are applied to a contract.
  - `"PayoutsWithdrawn"` when role payouts are withdrawn from a payout validator.
- Performance of marlowe-finder improved significantly.
