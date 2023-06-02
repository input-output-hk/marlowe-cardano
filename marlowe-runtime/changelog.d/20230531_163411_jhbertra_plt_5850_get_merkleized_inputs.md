### Added

- `MarkleizeInputs` request to `ContractRequest` (`marlowe-contract` query API)

### Changed

- `Apply` will auto-merkleize normal inputs to a contract if the contract is in
  the store.
  - E.g. Given an on-chain contract `When [MerkleizedCase (Notify TrueObs) "foo"] 0 Close`,
    a client can call `Apply` with `[NormalInput INotify]` and `marlowe-tx` will
    merkleize it automatically if it can find the contract in the store.
    Clients can still pass manually merkleized inputs to `Apply` if desired.
