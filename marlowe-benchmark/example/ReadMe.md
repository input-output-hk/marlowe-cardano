# Benchmarking Marlowe on a selected network


This example walks through executing and plotting a complete set of benchmarks for one of the selected networks (`preview` | `preprod` | `sanchonet` | `mainnet`).

## Prerequisites

The following tools must be on the `PATH`:

- `podman`: A tool for managing containers, used to manage the runtime containers for benchmarking.
- `cardano-cli`: The command-line interface for interacting with the Cardano blockchain.
- `marlowe-benchmark`: A benchmarking tool for Marlowe, a domain-specific language for financial contracts on Cardano.
- `psql`: The command-line interface for interacting with PostgreSQL, used for managing the database.
- `jq`: A lightweight and flexible command-line JSON processor, used for manipulating JSON data.
- `dasel`: A command-line tool for querying and modifying JSON and YAML data structures.
- `gawk`: The GNU implementation of the AWK programming language, used for text processing.
- `sed`: A stream editor for filtering and transforming text, used for text processing.
- `bc`: A command-line calculator, used for performing arithmetic calculations.
- `curl`: A command-line tool for making HTTP requests, used for downloading files from a URL.

## Magic Number Reminder 
- `mainnet`: 764824073
- `preprod`: 1
- `preview`: 2
- `sanchonet`: 4

# Select the network and fetch network configs

To begin, select the network you want to benchmark (`sanchonet`, `preview`, `preprod`) and fetch the network configuration files.

```bash
NETWORK_NAME=preprod ## Select the one you want to benchmark {sanchonet, preview, preprod, mainnet}
NETWORK_MAGIC_NUMBER=1
cd $NETWORK_NAME
rm -rf config
mkdir -p config
for f in {config,topology,byron-genesis,shelley-genesis,alonzo-genesis}.json
do
  curl "https://raw.githubusercontent.com/IntersectMBO/cardano-world/master/docs/environments/$NETWORK_NAME/$f" -o config/$f
done
mv config/{config,node}.json
```
**Note**: Stay in the current folder until the end of the process.

## 1. Create the database folders

```bash
for f in pg.db node.db contract.db
do
  if [ -d $f ]
  then
    sudo rm -rf $f/*
  else
    mkdir $f
  fi
done
```

## 2. Set the Faucet for running the downstream Txs

### 2.1 Generate the Verification, Signing Key  + Payment Adress



```shell
rm -rf faucet;
mkdir faucet;
cd faucet;
cardano-address recovery-phrase generate --size 15 > phrase.prv;
cardano-address key from-recovery-phrase Shelley < phrase.prv > root.xsk
cat root.xsk | cardano-address key child 1852H/1815H/0H/0/0 > addr.xsk
cat addr.xsk | cardano-address key public --without-chain-code > addr.pub
cardano-address key child 1852H/1815H/0H/0/0 < root.xsk | cardano-address key public --with-chain-code > addr.xvk
cardano-address address payment --network-tag testnet < addr.xvk > payment.addr
cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file addr.xsk --out-file addr.skey
cardano-cli key verification-key --signing-key-file addr.skey --verification-key-file addr.vkey

echo "Please Provision with ADA this address :  $(cat payment.addr)"
cd ..
```
### 2.2 Provisionning the account 


##### Generating a Payment adress 

```shell
cardano-cli address build \
    --payment-verification-key-file `faucet/payment.vkey` \
    --out-file `faucet/ayment.addr` \
    --testnet-magic $NETWORK_MAGIC_NUMBER
```

##### Provisioning the Payment adress with the faucet

Here are the faucet webpages for obtaining test ADA on various Cardano networks:

1. **Pre-production Testnet**:
   - **Faucet URL**: [Pre-production Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/)

2. **Preview Testnet**:
   - **Faucet URL**: [Preview Testnet Faucet](https://faucet.preview.world.dev.cardano.org)

3. **SanchoNet**:
   - **Faucet URL**: [SanchoNet Faucet](https://faucet.sancho.network)


These faucets allow developers and testers to request test ADA for use in these respective test environments. For more details on using these faucets, you can visit the official [Cardano documentation](https://docs.cardano.org/cardano-testnets/environments/)

if you have `cardano-cli` available :

```shell
cardano-cli query utxo \
    --address  $(cat faucet/payment.addr)  \
    --testnet-magic $NETWORK_MAGIC_NUMBER
```
or look at a Cardano explorer :  e.g cardano scan

To create and submit a transaction to the selected network, you'll need to provide the FAUCET details through two environment variables :

```shell
export FAUCET_ADDRESS=$(cat faucet/payment.addr);
export FAUCET_SKEY="$(pwd)/faucet/addr.skey";
```

### 2.3 Sourcing Environement variables

```bash
source environment
```
## 3. Create the pod and containers

**Note**: The runtime containers for benchmarking are managed by Podman.

```bash
podman play kube --replace=true --start=false pod-compose.yaml
```

## 4. Measure the performance of syncing from genesis

```bash
../measure-sync.sh
```

```console
--- Environment variables ---
PODNAME=benchmark-sanchonet
DB_PORT=54321
CARDANO_NODE_SOCKET_PATH=node.socket
CARDANO_TESTNET_MAGIC=4
MAGIC=--testnet-magic 4
SYNC_PODSTAT_FILE=sync-podstat.json
SYNC_RESULT_FILE=sync-results.tsv

--- Tools ---
podman version 4.7.2
cardano-cli 8.1.2 - linux-x86_64 - ghc-8.10
psql (PostgreSQL) 14.9
jq-1.6
GNU Awk 5.2.2, API 3.2, PMA Avon 8-g1

Starting container benchmark-sanchonet-postgres
Waiting for postgresql

Starting container benchmark-sanchonet-node
Waiting for node socket

Node sync: 99.99%

Starting container benchmark-sanchonet-chain-indexer
Waiting for chain schema
Chain indexer lag: 0 slots

Starting container benchmark-sanchonet-chain-sync

Starting container benchmark-sanchonet-marlowe-indexer
Waiting for marlowe schema
Marlowe indexer lag: 0 slots

Starting remaining containers in pod b9463cbd7f198bf43ceb38869b2e9b4d4b0c385b7ab48a5efde5082038ba84c5
```


## 5. Optionally, restart the pod to free up memory held by the syncing from genesis

```bash
podman pod stop "benchmark-$NETWORK_NAME"
podman pod start "benchmark-$NETWORK_NAME"

```


## 6. Measure the performance of executing benchmarks

Install the last version of marlowe-benchmark you have in this repo : 

```bash
cabal install marlowe-benchmark --overwrite-policy=always
```

```bash
../measure-benchmark.sh
```

```console
PODNAME=benchmark-sanchonet
NETWORK=sanchonet
BENCHMARK_CONFIG=../benchmark-config.json
MARLOWE_RT_HOST=oryx
MARLOWE_RT_PORT=37001
CARDANO_NODE_SOCKET_PATH=node.socket
CARDANO_TESTNET_MAGIC=1
FAUCET_ADDRESS=addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
FAUCET_SKEY=/extra/iohk/networks/treasury/payment.skey
BENCHMARK_PODSTAT_FILE=benchmark-podstats.json
BENCHMARK_RESULT_FILE=benchmark-results.json

podman version 4.7.2
cardano-cli 8.1.2 - linux-x86_64 - ghc-8.10
marlowe-benchmark 0.1.0.0
jq-1.6
GNU Awk 5.2.2, API 3.2, PMA Avon 8-g1

HeaderSync: 2024-01-20 15:37:21.444317475 UTC
BulkSync: 2024-01-20 15:47:17.848871186 UTC
Sync: 2024-01-20 15:48:14.023539636 UTC
Query: 2024-01-20 15:53:31.520253788 UTC
Lifecycle: 2024-01-20 15:57:40.60534639 UTC
Generated address "60798ab340704aaa47aeab82b9d0be26ebaad7ab943585fd9da93339fa" and signing key "5872a7683cd6b5b33f1511f45fce8ccaedb45ab9c89a222aaa3fc005b7f2e04934cf60e9e4c9146e86c4facb6b01ca9d3e82dd170a606099d993ee3dac357942705d79223f1238048ac33e59a284f8a3577261ef360bcb4ac6a6bedde624316a063eef43d14f0bb190b9f2a53f258d862e76598f174b1daa8518389327c90305".
Generated address "60fba8ad0d3f37f42916065475402cddd008ecd6409429921358a918fd" and signing key "98bc59e028c0ace15c2f238f5c440e3c864a31212824654f1d39d5e95a67744c270787db10b5014b28fb6ee9527d36795bd2bf537e8030d0f8e2f27ccdd5ef63b7429cfb6ba278cad74132ba18d1edb6938b89abe07a472cb4562638ed87454b8d9d0633e41652302792549d86d6f3b475c252e5703f30ef026c3c34b93992c7".
Generated address "609f9f8f4805020325eda5f7c612cdefe514b9a2c52dfc001526cf587b" and signing key "80600d0d902932355da4bf0a0faf555727e13edf323a9604e02026acf250fe5d507ebc57f69b66b9f31f50c285bde19a39adb03b556c1f72a4dffbb72dc0b1f0c5299d2e4cac1f1bb7aa64f48999341a85f5f0070d48f377c9d2edeefe7059e747d0d864c0b47d4480655d40fd15f3a063bbddaa90c8bbe54bfef90c22603de1".
Generated address "605f1d7c31d7946c3dff2a3272ad4a9a0f37e614dcc8864337ae4e10af" and signing key "004b61e2c06d982e73e50d9075465b85a775dcf936347244de5ef060504b535f9c29668163ce81f6699ab17fe098c9c4bb1b7ec5e8fd190cc8db95a37349909495851ed89037402a24df99d238c550245c2f5d7e78615a40db4322ea52e512fc7460ca64c78c2161c5079ad67065c2f4133e2561d45fcda3aafdefcabf1ba06a".
Done: 2024-01-20 16:02:21.70079731 UTC
```


## 7. Measure the disk usage

```bash
../measure-disk.sh
```


## 8. Make the plots

```bash
../plot-sync.R
../plot-benchmarks.R
```


