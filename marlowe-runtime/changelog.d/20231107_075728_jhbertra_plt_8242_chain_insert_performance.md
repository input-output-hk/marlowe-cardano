### Added

- `marlowe-chain-copy` executable for efficiently seeding a new chain database.

### Changed

- Dropped `asset` table from `chain` schema, inlining fields into `assetOut`
  and `assetMint`.
- Improved sync performance of `marlowe-chain-indexer` by switching from
  `INSERT` to `COPY`.
- Internal: consolidated database row extraction logic for cardano blocks into
  reusable modules.
