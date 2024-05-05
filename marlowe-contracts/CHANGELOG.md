
<a id='changelog-1.0.0.0'></a>
# 1.0.0.0 — 2024-04-22

## Changed

- Upgrade `cardano-api` `8.37.1.1` to `8.39.2.0` (Conway Hard Fork Adaptation)
- Upgrade `plutus` from `1.15` to `1.21` (`plutus-core`, `plutus-ledger-api`, `plutus-tx`) (Conway Hard Fork Adaptation)
- Upgrade `cardano-ledger` (Conway Hard Fork Adaptation)
    - `cardano-ledger-conway` from `1.11` to `1.12`
    - `cardano-ledger-core` from `1.9` to `1.10`
    - `cardano-ledger-mary` from `1.4` to `1.5`
    - `cardano-ledger-shelley` from `1.8` to `1.9`
    -  `cardano-ledger-binary` from `1.2` to `1.3`
- Upgrade `ouroboros-network` (Conway Hard Fork Adaptation)
    - `ouroboros-network-api` from `0.6.0` to `0.7.0`
    - `ouroboros-consensus` from `0.14` to `0.16`
    - `ouroboros-network-protocols` from `0.6.0` to `0.8`

<a id='changelog-0.2.0.0'></a>
# 0.2.0.0 — 2023-09-22

## Added

- Raffle contract.
- Chunked value deposit contract.

## Changed

- `Future` contract defined as bundle
