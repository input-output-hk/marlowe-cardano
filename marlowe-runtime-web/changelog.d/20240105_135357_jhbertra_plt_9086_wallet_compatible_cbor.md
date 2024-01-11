### Removed

- `TxBody` format in `POST` responses (only `Unwitnessed Tx` responses supported).
- Full `Tx` format in `PUT` requests (only `TxWitness` responses supported).

### Changed

- `Tx <era>` format in `POST` responses changed to `Unwitnessed Tx <era>`
- Transactions in `POST` responses are CDDL compliant.
- `ShelleyTxWitness <era>` format in `PUT` requests changed to `TxWitness Set <era>`
- TxWitnesses in `PUT` requests must be CDDL compliant.

### Fixed

- Runtime REST API does not return CDDL-compatible transaction CBOR.
