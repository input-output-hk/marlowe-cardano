# Deploying Marlowe Runtime Backend Services Manually

Deploying the Marlowe Runtime requires running seven backend services:
- five [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc) services:
	- `marlowe-chain-indexer` for indexing the blockchain
	- `marlowe-chain-sync` for querying the indexed blockchain
	- `marlowe-indexer` for indexing Marlowe contracts
	- `marlowe-sync` for tracking the on-chain history of all Marlowe contract instances
	- `marlowe-tx` for building and submitting Marlowe transactions
- [cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/README.rst) for blockchain connectivity
- [PostgreSQL](https://www.postgresql.org/) for persistent storage

This document describes how to deploy manually, on a service-by-service basis. For deployment using Docker, see [here](docker.md).


## Building the Executables

See the [cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/README.rst) and [PostgreSQL](https://www.postgresql.org/) websites for instructions on obtaining or building those products.

Building the Marlowe Runtime backend components requires the [Nix package manager](https://nixos.org). Visit https://nixos.org or [Cardano-specific instructions](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc#nix-advice) for guidance on setting up Nix.

Provided that Nix is already installed, first clone the [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc) git repository:
```bash
git clone https://github.com/input-output-hk/marlowe-cardano.git
```

The backend components are available from the Nix Flake as apps. Note the `--`
separating the arguments from the command.
```console
$ cd marlowe-cardano

$ nix run .#marlowe-chain-indexer -- --help
$ nix run .#marlowe-chain-sync -- --help
$ nix run .#marlowe-indexer -- --help
$ nix run .#marlowe-sync -- --help
$ nix run .#marlowe-tx -- --help
$ nix run .#marlowe -- --help
```

For convenience, several Cardano tools and other utilities are built and available in the development shell.
```console
$ which cardano-node cardano-cli cardano-address sqitch
/nix/store/...-cardano-node-exe-cardano-node-.../bin/cardano-node
/nix/store/...-cardano-cli-exe-cardano-cli-.../bin/cardano-cli
/nix/store/...-cardano-addresses-cli-exe-cardano-address-.../bin/cardano-address
/nix/store/...-sqitch-pg-.../bin/sqitch
```

## Configuring the Backend Services


### Cardano Node

Start the Cardano node for the network of your choice: see https://book.world.dev.cardano.org/environments.html for the current list of available networks.


### PostgreSQL Database

Select a database name for the Marlowe chain sync database and create it using PostgreSQL's `createdb` command. Edit the file [marlowe-chain-sync/sqitch.conf](../../marlowe-chain-sync/sqitch.conf) file to reflect the PostgreSQL user and database names for your PostgreSQL installation.

Database performance on `mainnet` will be slow unless the write-ahead log and other parameters in the `postgresql.conf` file are tuned:
```console
shared_buffers = 2GB
huge_pages = try
temp_buffers = 2GB
max_prepared_transactions = 256
max_wal_size = 4GB
max_pred_locks_per_transaction = 256
```


## Running the Backend Services

Start the backend services in the following order.
- PostgreSQL
- Cardano Node
- `marlowe-chain-indexer`
- `marlowe-chain-sync`
- `marlowe-indexer`
- `marlowe-sync`
- `marlowe-tx`


### Marlowe Chain Sync Daemon

See the [help page](marlowe-chain-sync.md) for all of the command-line options for `marlowe-chain-sync`. The port numbers have sensible default values that are consistent with the other Marlowe Runtime services, but one needs to specify a few options explicitly:
- `--socket-path` for the filesystem path to the Cardano node's socket.
- `--database-uri` for the location and name of the PostgreSQL database.

A typical invocation of `marlowe-chain-sync` will be like something along the following lines:
```console
$ marlowe-chain-sync \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --database-uri postgresql://postgresql@0.0.0.0/chain
```


### Marlowe Chain Indexer Daemon

See the [help page](marlowe-chain-indexer.md) for all of the command-line options for `marlowe-chain-indexer`. One needs to specify a few options explicitly:
- `--socket-path` for the filesystem path to the Cardano node's socket.
- `--database-uri` for the location and name of the PostgreSQL database.
- `--genesis-config-file` for the filesystem path to the genesis file used by the Cardano node.
- `--shelley-genesis-config-file` for the filesystem path to the shelley genesis file used by the Cardano node.
- `--genesis-config-file-hash` for the hash of the genesis file.

A typical invocation of `marlowe-chain-indexer` will be like something along the following lines:
```console
$ cd marlowe-chain-sync
$ sqitch deploy

$ marlowe-chain-indexer \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --database-uri postgresql://postgresql@0.0.0.0/chain \
    --genesis-config-file byron-genesis.json \
    --shelley-genesis-config-file shelley-genesis.json \
    --genesis-config-file-hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb
```
The `sqitch deploy` command handles database creation and migration, so it is only necessary to run that on new installations or after upgrading `marlowe-chain-sync`.
To be safe, consider running this command before each invocation of `marlowe-chain-indexer`.


### Marlowe Indexer Daemon

See the [help page](marlowe-indexer.md) for all of the command-line options for `marlowe-indexer`. One needs to specify a few options explicitly:
- `--database-uri` for the location and name of the PostgreSQL database.

A typical invocation of `marlowe-indexer` will be like something along the following lines:
```console
$ cd marlowe-runtime/marlowe-indexer
$ sqitch deploy

$ marlowe-indexer \
    --database-uri postgresql://postgresql@0.0.0.0/chain \
```
The `sqitch deploy` command handles database creation and migration, so it is only necessary to run that on new installations or after upgrading `marlowe-indexer`.
To be safe, consider running this command before each invocation of `marlowe-indexer`.


### Marlowe Sync

See the [help page](marlowe-sync.md) for all of the command-line options for `marlowe-sync`. One needs to specify a few options explicitly:
- `--database-uri` for the location and name of the PostgreSQL database.

A typical invocation of `marlowe-sync` will be like something along the following lines:
```console
$ marlowe-sync \
    --database-uri postgresql://postgresql@0.0.0.0/chain \
```


### Marlowe Tx

See the [help page](marlowe-tx.md) for all of the command-line options for `marlowe-tx`. The default values for port numbers are consistent with those for `marlowe-chain-sync`, so they typically do not need to be specified explicitly. A typical invocation of `marlowe-tx` is simple:
```console
$ marlowe-tx
```

### Marlowe Proxy

See the [help page](marlowe-proxy.md) for all of the command-line options for `marlowe-proxy`. The default values for port numbers are consistent with those for `marlowe-sync` and `marlowe-tx`, so they typically do not need to be specified explicitly. A typical invocation of `marlowe-proxy` is simple:
```console
$ marlowe-proxy
```


## Checking the Deployment

TODO
