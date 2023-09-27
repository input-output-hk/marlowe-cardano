# Recommended Resources for Deploying Marlowe Runtime

This document summarizes recommendations for the computational resources required to deploy Marlowe Runtime on public networks (`mainnet`, `preprod`, and `preview`).

For more information about how to get started using the Cardano testnets, please see [Getting started](https://docs.cardano.org/cardano-testnet/getting-started).


## Marlowe Services

The table below provides guidance on the memory and CPU resources required for Marlowe Runtime's services. These are based on peak usages over the following disparate circumstances across all three public networks:

1. Indexing the blockchain from genesis to the tip.
2. Running 500 Marlowe contracts in parallel at the maximum possible transaction-building and -submission rate.
3. Querying the history of all Marlowe contracts at the maximum rate.

| Service                 | Memory  | CPU (cores) |
|-------------------------|--------:|------------:|
| `marlowe-chain-indexer` | 4000 MB |       100 % |
| `marlowe-chain-sync`    | 2500 MB |        25 % |
| `marlowe-indexer`       | 2000 MB |       100 % |
| `marlowe-sync`          |   50 MB |        25 % |
| `marlowe-contract`      |   25 MB |        50 % |
| `marlowe-tx`            | 1000 MB |       100 % |
| `marlowe-proxy`         |  100 MB |        50 % |
| `marlowe-web-server`    |   50 MB |        10 % |
| Total                   | 9000 MB |       500 % |


## Sync times

On a 32-core Intel i9 with 4000 MHz dual-channel DDRS memory and 7000 MB/s SSDs, the following times for syncing the Marlowe schemas from genesis were observed.

| Service                 | Mainnet | Preprod | Preview |
|-------------------------|--------:|--------:|--------:|
| `marlowe-chain-indexer` | 510 min |   7 min |  16 min |
| `marlowe-indexer`       |   3 min |   1 min |   2 min |


## PostgreSQL

The table below lists the maximum size of the Marlowe Runtime database schemas for the three public networks, as of March 2023. Because the public blockchains grow in size over time, disk resources will increase correspondingly.

| Schema    | `mainnet` | `preprod` | `preview` |
|-----------|----------:|----------:|----------:|
| `chain`   |    200 GB |     10 GB |      2 GB |
| `marlowe` |      2 MB |      5 MB |    300 MB |
| `sqitch`  |    200 kB |    200 kB |    200 kB |
| Total     |    200 GB |     10 GB |      3 GB |

PostgreSQL memory and CPU requirements depend upon the particular PostgreSQL installation. During indexing, the SQL queries for Marlowe Runtime typically require 100% of a CPU core. A small PostgreSQL installation could use as little as 2 GB of memory, but such an installation would be slow to index the blockchain, though it would be adequate for queries once the blockchain was fully indexed.

For fast indexing, about 8 GB of memory is ample for a PostgreSQL instance with the following custom configuration:

```console
max_connections = 1000;
superuser_reserved_connections = 5;
huge_pages = "try";
max_wal_size = "6GB";
max_locks_per_transaction = 256;
max_pred_locks_per_transaction = 256;
work_mem = "32MB";
maintenance_work_mem = "256MB";
```


### Cardano Node

See https://cardano-node-installation.stakepool247.eu/ for guidance on the CPU, memory, and disk requirements for running Cardano Node on public networks.
