### Added

- Assets to several response schemas
  - In `Payout` - shows the assets in the tx output corresponding to the payout.
  - In `ContractState` - shows the assets in the current UTxO of the contract (the account balances). Empty if the contract has no UTxO (i.e. the contract is closed).
  - In `Tx` - shows the assets in the marlowe script output of that transaction (the account balances). Empty if the transaction does not produce an output.
