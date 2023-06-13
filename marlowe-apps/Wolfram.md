

# Oracle Setup

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

Download and Install Cardano node

```
cardano-node run --socket-path cardano.sock --config config.json --topology topolgy.json

cardano-cli query tip —testnet-magic 2 --socket-path cardano.sock
#syncProgress has to be 100%

cardano-cli address key-gen \
    --extended-key \
    --verification-key-file ~/keys/payment.vkey \
    --signing-key-file ~/keys/payment.skey

cardano-cli key non-extended-key --extended-verification-key-file ~/keys/payment.vkey --verification-key-file ~/keys/nonextended.vkey

cardano-cli address build \
    --payment-verification-key-file ~/keys/payment.vkey \
    --out-file ~/keys/payment.addr \
    --testnet-magic 2

```


## RUNNING ORACLE

1. Prepare Marlowe Runtime Environment
[example](https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/doc/tutorial.ipynb)
2. Run Oracle

```
cabal run marlowe-oracle -- addr_test1vrxx3rjangevudlrejgp9m508uc26jd02n7lmdz365n4wrca35y5j ~/keys/payment.skey
```

## RUNNING TESTS

```

cardano-cli query utxo \
--address addr_test1vrxx3rjangevudlrejgp9m508uc26jd02n7lmdz365n4wrca35y5j \
--testnet-magic 2 \
--socket-path cardano.sock 

marlowe-cli test ~/marlowe-cardano/marlowe-cli/test/operations/mint.yaml --testnet-magic 2 --socket-path cardano.sock --faucet-skey-file ~/keys/payment.skey --faucet-address addr_test1vrxx3rjangevudlrejgp9m508uc26jd02n7lmdz365n4wrca35y5j

```

https://docs.cardano.org/cardano-testnet/tools/faucet/

## USEFUL LINKS

https://preview.cardanoscan.io/

https://github.com/input-output-hk/marlowe-cardano/blob/5526fb77f12b61213c595ba254c412792e4749ee/nix/dev/compose.nix

https://github.com/input-output-hk/marlowe-cardano/blob/5526fb77f12b61213c595ba254c412792e4749ee/marlowe-runtime/doc/deployment.md

