#!/usr/bin/env bash


####
#### Example for using `marlowe-cli` to run Marlowe contracts on `testnet`.
####
#### This uses the `address`, `validator`, `data`, `redeemer` commands of `cardano-cli`.
####


# Make sure that cardano-cli is on the path!


# Select the network.

NETWORK=testnet
MAGIC="--testnet-magic 1097911063"

cardano-cli query protocol-parameters "$MAGIC" --out-file $NETWORK.protocol


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=addr_test1qq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60slacjyp
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Set the file names.

PLUTUS_FILE=test.plutus
DATUM_FILE=test.datum
REDEEMER_FILE=test.redeemer


# Configure the contract.

CONTRACT_FILE=example.contract
STATE_FILE=example.state
DATUM_LOVELACE=$(jq '.accounts | .[0] | .[1]' $STATE_FILE)
REDEEMER_MIN_SLOT=1000
REDEEMER_MAX_SLOT=43500000


# Create the contract.

ADDRESS_S=$(marlowe-cli address "$MAGIC")

marlowe-cli validator "$MAGIC" --out-file $PLUTUS_FILE

DATUM_HASH=$(
marlowe-cli datum --contract-file $CONTRACT_FILE \
                  --state-file $STATE_FILE       \
                  --out-file $DATUM_FILE
)

marlowe-cli redeemer --min-slot $REDEEMER_MIN_SLOT \
                     --max-slot $REDEEMER_MAX_SLOT \
                     --out-file $REDEEMER_FILE


# Find funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo "$MAGIC" --address $ADDRESS_P

TX_0=0faea72be3516b952e0552d9f8643386b969124e385073aa4e7729f92fe53a52


# Fund the contract.

cardano-cli transaction build --alonzo-era "$MAGIC"                 \
                              --tx-in $TX_0#0                     \
                              --tx-out "$ADDRESS_S"+"$DATUM_LOVELACE" \
                                --tx-out-datum-hash "$DATUM_HASH"   \
                              --change-address $ADDRESS_P         \
                              --out-file tx.raw

cardano-cli transaction sign "$MAGIC"                          \
                             --tx-body-file tx.raw           \
                             --signing-key-file payment.skey \
                             --out-file tx.signed

cardano-cli transaction submit "$MAGIC" --tx-file tx.signed


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo "$MAGIC" --address "$ADDRESS_S"

TX_1=276633f3b74378f1c616e94dd3365ef67b99a5377f7f5a27938ee3febdab459f

FUNDS=$(cardano-cli query utxo "$MAGIC" --address $ADDRESS_P --out-file /dev/stdout | jq '.["'$TX_1#0'"].value.lovelace')


# Redeem the contract.

FEE=$(
cardano-cli transaction build --alonzo-era "$MAGIC"                      \
                              --protocol-params-file $NETWORK.protocol \
                              --tx-in $TX_1#1                          \
                                --tx-in-script-file $PLUTUS_FILE       \
                                --tx-in-datum-file $DATUM_FILE         \
                                --tx-in-redeemer-file $REDEEMER_FILE   \
                              --tx-in $TX_1#0                          \
                              --tx-out $ADDRESS_P+"$DATUM_LOVELACE"      \
                              --change-address $ADDRESS_P              \
                              --tx-in-collateral $TX_1#0               \
                              --invalid-before $REDEEMER_MIN_SLOT      \
                              --invalid-hereafter $REDEEMER_MAX_SLOT   \
                              --out-file tx.raw                        \
| sed -e 's/^.* //'
)

NET=$((DATUM_LOVELACE + FUNDS - FEE))

cardano-cli transaction build --alonzo-era "$MAGIC"                      \
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

cardano-cli transaction sign "$MAGIC"                          \
                             --tx-body-file tx.raw           \
                             --signing-key-file payment.skey \
                             --out-file tx.signed

cardano-cli transaction submit "$MAGIC" --tx-file tx.signed


# See that the transaction succeeded.

cardano-cli query utxo "$MAGIC" --address "$ADDRESS_S"

cardano-cli query utxo "$MAGIC" --address $ADDRESS_P

#### Voilà! See <https://testnet.cardanoscan.io/transaction/a586813b1da04fc9092303dfa14f3d7a3759d3de594e6defd29963dc0e4edeaa>.

