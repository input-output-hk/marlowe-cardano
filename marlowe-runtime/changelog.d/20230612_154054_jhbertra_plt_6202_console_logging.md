### Changed

- Console log formatting.
- Increased the maximum confirmation wait time in `marlowe-tx` to 1 hour.

### Fixed

- Console logs are new thread-safe.
- `marlowe-scaling` did not delay polling at all when submitting a transaction.
