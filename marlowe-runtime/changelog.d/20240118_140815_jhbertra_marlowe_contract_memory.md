### Fixed

- `marlowe-contract` consumes a lot of memory.
  - The garbage collector was holding onto a rolling window of snapshots
    spanning 2160 blocks of all live Marlowe contracts. It only ever needed to
    hold onto the datum hashes of the merkleized continuations in the
    contracts, so rather than keeping all the contracts around it now just
    holds onto these.
