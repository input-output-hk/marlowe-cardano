#!/usr/bin/env bash


####
#### Example for using `marlowe-cli` to run Marlowe contracts on `testnet`.
####
#### This uses the `export` command of `cardano-cli`, in conjuction with `jq`.
####


# Make sure that cardano-cli is on the path and that jq is installed!


# Select the network.

NETWORK=testnet
MAGIC="--testnet-magic 1097911063"

cardano-cli query protocol-parameters $MAGIC --out-file $NETWORK.protocol


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Set the file name.

MARLOWE_FILE=test.marlowe


# Configure the contract.

CONTRACT_FILE=example.contract
STATE_FILE=example.state
DATUM_LOVELACE=$(jq '.accounts | .[0] | .[1]' $STATE_FILE)
REDEEMER_MIN_SLOT=1000
REDEEMER_MAX_SLOT=43500000


# Create the contract, and extract the address, validator, datum hash, datum, and redeemer.

marlowe-cli export $MAGIC                                 \
                   --contract-file $CONTRACT_FILE         \
                   --state-file $STATE_FILE               \
                   --redeemer-min-slot $REDEEMER_MIN_SLOT \
                   --redeemer-max-slot $REDEEMER_MAX_SLOT \
                   --out-file $MARLOWE_FILE               \
                   --print-stats

ADDRESS_S=$(jq -r '.validator.address' $MARLOWE_FILE)

PLUTUS_FILE=test.plutus
jq '.validator.script' $MARLOWE_FILE > $PLUTUS_FILE

DATUM_HASH=$(jq -r '.datum.hash' $MARLOWE_FILE)

DATUM_JSON_FILE=test.datum
jq '.datum.json' $MARLOWE_FILE > $DATUM_JSON_FILE

REDEEMER_FILE=test.redeemer
jq '.redeemer.json' $MARLOWE_FILE > $REDEEMER_FILE


# Find funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo $MAGIC --address $ADDRESS_P

TX_0=06f9f217094875b3208fcc2c2b6549055c351aff7a3525a4b1fea94cb2f7f0c0


# Fund the contract.

cardano-cli transaction build --alonzo-era $MAGIC                 \
                              --tx-in $TX_0#0                     \
                              --tx-out $ADDRESS_S+$DATUM_LOVELACE \
                                --tx-out-datum-hash $DATUM_HASH   \
                              --change-address $ADDRESS_P         \
                              --out-file tx.raw

cardano-cli transaction sign $MAGIC                          \
                             --tx-body-file tx.raw           \
                             --signing-key-file payment.skey \
                             --out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo $MAGIC --address $ADDRESS_S

TX_1=ba3c61139c19d0067366338776cfbb3d8bf615abe0391871896ad62c5a7418db

FUNDS=$(cardano-cli query utxo $MAGIC --address $ADDRESS_P --out-file /dev/stdout | jq '.["'$TX_1#0'"].value.lovelace')


# Redeem the contract.

FEE=$(
cardano-cli transaction build --alonzo-era $MAGIC                      \
                              --protocol-params-file $NETWORK.protocol \
                              --tx-in $TX_1#1                          \
                                --tx-in-script-file $PLUTUS_FILE       \
                                --tx-in-datum-file $DATUM_FILE         \
                                --tx-in-redeemer-file $REDEEMER_FILE   \
                              --tx-in $TX_1#0                          \
                              --tx-out $ADDRESS_P+$DATUM_LOVELACE      \
                              --change-address $ADDRESS_P              \
                              --tx-in-collateral $TX_1#0               \
                              --invalid-before $REDEEMER_MIN_SLOT      \
                              --invalid-hereafter $REDEEMER_MAX_SLOT   \
                              --out-file tx.raw                        \
| sed -e 's/^.* //'
)

NET=$((DATUM_LOVELACE + FUNDS - FEE))

cardano-cli transaction build --alonzo-era $MAGIC                      \
                              --protocol-params-file $NETWORK.protocol \
                              --tx-in $TX_1#1                          \
                                --tx-in-script-file $PLUTUS_FILE       \
                                --tx-in-datum-file $DATUM_FILE         \
                                --tx-in-redeemer-file $REDEEMER_FILE   \
                              --tx-in $TX_1#0                          \
                              --tx-out $ADDRESS_P+$NET                 \
                              --change-address $ADDRESS_P              \
                              --tx-in-collateral $TX_1#0               \
                              --invalid-before $REDEEMER_MIN_SLOT      \
                              --invalid-hereafter $REDEEMER_MAX_SLOT   \
                              --out-file tx.raw

cardano-cli transaction sign $MAGIC                          \
                             --tx-body-file tx.raw           \
                             --signing-key-file payment.skey \
                             --out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed


# See that the transaction succeeded.

cardano-cli query utxo $MAGIC --address $ADDRESS_S

cardano-cli query utxo $MAGIC --address $ADDRESS_P

#### Voilà! See <https://testnet.cardanoscan.io/transaction/b4dc619a567370c59f6bbbab4234f96c96563ac26a40adafa7f66d661eb7875c>.

