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
CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cardano-cli address build $MAGIC --payment-verification-key-file $PAYMENT_VKEY)
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Set the file name.

MARLOWE_FILE=test.marlowe


# Configure the contract.

CONTRACT_FILE=example.contract
STATE_FILE=example.state
DATUM_LOVELACE=$(jq '.accounts | .[0] | .[1]' $STATE_FILE)
REDEEM_MIN_SLOT=1000
REDEEM_MAX_SLOT=43500000


# Create the contract, and extract the address, validator, datum hash, datum, and redeemer.

marlowe-cli export $MAGIC                         \
                   --contract-file $CONTRACT_FILE \
                   --state-file $STATE_FILE       \
                   --out-file $MARLOWE_FILE       \
                   --print-stats

ADDRESS_S=$(jq -r '.validator.address' $MARLOWE_FILE)

PLUTUS_FILE=test.plutus
jq '.validator.script' $MARLOWE_FILE > $PLUTUS_FILE

DATUM_HASH=$(jq -r '.datum.hash' $MARLOWE_FILE)

DATUM_FILE=test.datum
jq '.datum.json' $MARLOWE_FILE > $DATUM_FILE

REDEEMER_FILE=test.redeemer
jq '.redeemer.json' $MARLOWE_FILE > $REDEEMER_FILE


# Find funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo $MAGIC --address $ADDRESS_P

TX_0=3ed9cbe11b6308c5ede3ca8c9eb3a7ba1d7fe00a958dceb029f6c6219180235f


# Fund the contract.

marlowe-cli build-incoming $MAGIC                                  \
                           --socket-path $CARDANO_NODE_SOCKET_PATH \
                           --script-address $ADDRESS_S             \
                           --tx-out-datum-file $DATUM_FILE         \
                           --tx-out-value $DATUM_LOVELACE          \
                           --tx-in $TX_0#0                         \
                           --change-address $ADDRESS_P             \
                           --out-file tx.raw

cardano-cli transaction sign $MAGIC                           \
                             --tx-body-file tx.raw            \
                             --signing-key-file $PAYMENT_SKEY \
                             --out-file tx.signed

cardano-cli transaction submit "$MAGIC" --tx-file tx.signed


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo $MAGIC --address $ADDRESS_S

TX_1=9c6d992735fd68ebf4e689ca75160007ffbdb584d4d908a1ab763d4d764eed13


# Redeem the contract.

marlowe-cli build-outgoing $MAGIC                                  \
                           --socket-path $CARDANO_NODE_SOCKET_PATH \
                           --tx-in-script-file $PLUTUS_FILE        \
                           --tx-in-redeemer-file $REDEEMER_FILE    \
                           --tx-in-datum-file $DATUM_FILE          \
                           --tx-in-marlowe $TX_1#1                 \
                           --tx-in $TX_1#0                         \
                           --tx-in-collateral $TX_1#0              \
                           --tx-out $ADDRESS_P+$DATUM_LOVELACE     \
                           --change-address $ADDRESS_P             \
                           --invalid-before $REDEEM_MIN_SLOT       \
                           --invalid-hereafter $REDEEM_MAX_SLOT    \
                           --out-file tx.raw

cardano-cli transaction sign $MAGIC                           \
                             --tx-body-file tx.raw            \
                             --signing-key-file $PAYMENT_SKEY \
                             --out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed


# See that the transaction succeeded.

cardano-cli query utxo $MAGIC --address $ADDRESS_S

cardano-cli query utxo $MAGIC --address $ADDRESS_P

