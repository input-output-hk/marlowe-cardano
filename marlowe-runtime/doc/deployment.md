# Deploying Marlowe Runtime Backend Services Manually

Deploying the Marlowe Runtime requires running six backend services:
- four [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc) services:
	- `chainseekd` for indexing the blockchain
	- `marlowe-history` for tracking the on-chain history of Marlowe contract instances
	- `marlowe-discovery` for discovering the presence of Marlowe contract instances on the blockchain
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

Entering a Nix development shell will trigger a build of the of the backend components: can be built using the `nix-build` command.
```console
$ cd marlowe-cardano

$ nix develop

$ which chainseekd marlowe-history marlowe-discovery marlowe-tx
/nix/store/nxsbjx9il9bxacvqga3g0i0srxw3mw7z-marlowe-chain-sync-exe-chainseekd-0.0.0.0/bin/chainseekd
/nix/store/5r7pwjlxwyvkbg920bfba7r4ynda1zag-marlowe-runtime-exe-marlowe-history-0.0.0.0/bin/marlowe-history
/nix/store/2n4hy4cd8929b0acfnhjcfi76vhz0idq-marlowe-runtime-exe-marlowe-discovery-0.0.0.0/bin/marlowe-discovery
/nix/store/ydr8kzzbjav1hvyg231k0rm3h5scpshb-marlowe-runtime-exe-marlowe-tx-0.0.0.0/bin/marlowe-tx
```

For convenience, several Cardano tools and other utilities are also built and available in the development shell.
```console
$ which cardano-node cardano-cli cardano-address sqitch
/nix/store/n7kq07hwybc1jjrgby99jpagc9bv9l2l-cardano-node-exe-cardano-node-1.35.3/bin/cardano-node
/nix/store/d6wadhy8v10xh8wh4vp97xj525dy6j8l-cardano-cli-exe-cardano-cli-1.35.3/bin/cardano-cli
/nix/store/mynrw54yci7cy2mgqshk8q67a1hl4djg-cardano-addresses-cli-exe-cardano-address-3.11.0/bin/cardano-address
/nix/store/3r9khb9rd94mfgx78zkll8dlnd3jzfwf-sqitch-pg-1.1.0/bin/sqitch
```

## Configuring the Backend Services


### Cardano Node

Start the Cardano node for the network of your choice: see https://book.world.dev.cardano.org/environments.html for the current list of available networks.


### PostgreSQL Database

Select a database name for the Marlowe chain seek database and create it using PostgreSQL's `createdb` command. Edit the file [marlowe-chain-sync/sqitch.conf](../../marlowe-chain-sync/sqitch.conf) file to reflect the PostgreSQL user and database names for your PostgreSQL installation.

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
- `chainseekd`
- `marlowe-history`
- `marlowe-discovery`
- `marlowe-tx`


### Marlowe Chain Seek Daemon

See the [help page](chainseekd.md) for all of the command-line options for `chainseekd`. The port numbers have sensible default values that are consistent with the other Marlowe Runtime services, but one needs to specify a few options explicitly:
- `--socket-path` for the filesystem path to the Cardano node's socket.
- `--database-uri` for the location and name of the PostgreSQL database.
- `--genesis-config-file` for the filesystem path to the genesis file used by the Cardano node.
- `--genesis-config-file-hash` for the hash of the gensis file.

A typical invocation of `chainseekd` will be like something along the following lines:
```console
$ sqitch deploy

$ chainseekd \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --database-uri postgresql://postgresql@0.0.0.0/chain \                     
    --genesis-config-file byron-genesis.json \ 
    --genesis-config-file-hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb
```
The `sqitch deploy` command handles database creation and migration, so it is only necessary to run that on new installations or after upgrading `chainseekd`.


### Marlowe History

See the [help page](marlowe-history.md) for all of the command-line options for `marlowe-history`. The default values for port numbers are consistent with those for `chainseekd`, so they typically do not need to be specified explicitly. A typical invocation of `marlowe-history` is simple:
```console
$ marlowe-history
```

### Marlowe Discovery

See the [help page](marlowe-discovery.md) for all of the command-line options for `marlowe-discovery`. The default values for port numbers are consistent with those for `chainseekd` and `marlowe-history`, so they typically do not need to be specified explicitly. A typical invocation of `marlowe-discovery` is simple:
```console
$ marlowe-discovery
```

### Marlowe History

See the [help page](marlowe-tx.md) for all of the command-line options for `marlowe-tx`. The default values for port numbers are consistent with those for `chainseekd` and `marlowe-history`, so they typically do not need to be specified explicitly. A typical invocation of `marlowe-tx` is simple:
```console
$ marlowe-tx
```


## Checking the Deployment

A simple way to check a Marlowe Runtime deployment is to query the list of all Marlowe contract instance on the blockchain. This can be done with the `marlowe` command from within the development Nix shell:
```console
$ cd marlowe-cardano

$ nix deploy

$ marlowe ls --all | head
02811e36c6cdac4721b53f718c4a1406e09ef0d985f9ad6b7fd676769e2f866c#1
045d4ac6f50c57cade58fd697b907ff0418637ba1eadb36392e04d1c5d489b21#1
0552fc00860d35d45c09b4b43740193579adbeea5097cf33b62201332e3095ee#1
05574d9d1935d2a5d2e3cb15e79d46b076e4c81d85bd2e12940ed47b628001be#1
05990678496287262051fc831ff35903e383ffc564b943de76e78394a8ceb4cd#1
061d29872aae0580496aecc8bed3a7a11d743c55d24f6d2c5c03c630cf89e899#1
066727dc5d40a6df33bb88dea3d29a9c1bb9b68493870ef9a04d6cbffe11e21b#1
06718671646f66d1818fce1d2532a3bb2050a42df164c71c3306f9c67ba6f1ab#1
06ddf0488df24a26882abf9546a8d314994b768465246172f8b4492e8a820162#1
072b7513bfae663c993d21977e6b1c2231a6fb389d09595a1e3d4ee517b5a6ed#1
```
