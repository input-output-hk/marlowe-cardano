# Setup

## PREPARE MARLOWE-CLI

1. Git clone cardano-marlowe (wolfram branch)
2. Install NIX and configure it in accordance to cardano-marlowe page
3. Run:
```
	nix develop
```

It will put you in NIX environment where marlowe-cli is available

4. Prepare dependencies for marlowe-cli
```
    cabal update
```
   
5. Run CLI

```
	marlowe-cli
```

and wait for it to finish first-time setup

## PREPARE CARDANO NODE RUNTIME ENVIRONMENT

Download and Install Cardano node to ~/cardano-node

```
./cardano-node run --socket-path cardano.sock --config config.json --topology topolgy.json

./cardano-cli query tip —testnet-magic 2 --socket-path cardano.sock
#syncProgress has to be 100%

./cardano-cli address key-gen \
    --extended-key \
    --verification-key-file ~/keys/payment.vkey \
    --signing-key-file ~/keys/payment.skey

./cardano-cli key non-extended-key --extended-verification-key-file ~/keys/payment.vkey --verification-key-file ~/keys/nonextended.vkey

./cardano-cli address build \
    --payment-verification-key-file ~/keys/payment.vkey \
    --out-file ~/keys/payment.addr \
    --testnet-magic 2

```


## PREPARE MARLOWE RUNTIME WITHOUT DOCKER ON MAC

1. Install telemetry collector
https://github.com/open-telemetry/opentelemetry-collector-releases/releases/tag/v0.79.0

2. Run it:
```
./otelcol-contrib --config ~/marlowe-cardano/otel/otel-collector-config.yml 
```

3. Install Postgress and create chain table for ‘postgress’ user

4. Install sqitch and initialize the schema:
```
brew tap sqitchers/sqitch
brew install sqitch --with-postgres-support --with-sqlite-support
cd ~/marlowe-cardano/marlowe-chain-sync

sqitch deploy  # deploys schema to Postgres’s

cd ~/marlowe-cardano/marlowe-indexer
sqitch deploy

```

5. Run services:
```
cabal run marlowe-chain-sync:marlowe-chain-sync -- --testnet-magic 2 -s ~/cardano-node/cardano.sock --database-uri postgresql://postgres@0.0.0.0/chain

cabal run marlowe-chain-indexer -- -s ~/cardano-node/cardano.sock --database-uri postgresql://postgres@0.0.0.0/chain --genesis-config-file-hash 83de1d7302569ad56cf9139a41e2e11346d4cb4a31c00142557b6ab3fa550761 --genesis-config-file ~/cardano-node/byron-genesis.json --shelley-genesis-config-file ~/cardano-node/shelley-genesis.json --testnet-magic 2

cabal run marlowe-indexer -- --database-uri postgresql://postgres@0.0.0.0/chain --http-port 9090

cabal run marlowe-sync -- --database-uri postgresql://postgres@0.0.0.0/chain --http-port 9393

cabal run marlowe-tx -- --http-port 9191 &

cabal run marlowe-proxy -- --http-port 9292 &

cabal run marlowe-contract -- --http-port 9797 &

```

## RUNNING ORACLE

```
cabal run marlowe-oracle -- addr_test1vrxx3rjangevudlrejgp9m508uc26jd02n7lmdz365n4wrca35y5j ~/keys/payment.skey
```

## RUNNING TESTS

```
marlowe-cli test ~/marlowe-cardano/marlowe-cli/test/operations/mint.yaml --testnet-magic 2 --socket-path ~/cardano-node/cardano.sock  --faucet-skey-file ~/keys/payment.skey --faucet-address addr_test1vrxx3rjangevudlrejgp9m508uc26jd02n7lmdz365n4wrca35y5j

```

## USEFUL LINKS
https://www.reddit.com/r/cardano/comments/13nmxno/problem_running_cardano_node/
https://preview.cardanoscan.io/

https://developers.cardano.org/docs/operate-a-stake-pool/generating-wallet-keys/


https://book.world.dev.cardano.org/environments.html#vasil-dev



https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/doc/tutorial.ipynb

https://github.com/input-output-hk/marlowe-cardano/blob/5526fb77f12b61213c595ba254c412792e4749ee/nix/dev/compose.nix

https://github.com/input-output-hk/marlowe-cardano/blob/5526fb77f12b61213c595ba254c412792e4749ee/marlowe-runtime/doc/deployment.md



