#!/usr/bin/env bash


####
#### Example for using `marlowe-cli` to run Marlowe contracts on `testnet`.
####
#### This uses the `export` command of `cardano-cli`, in conjuction with `jq`.
####


# Make sure that cardano-cli is on the path and that jq is installed!


# Select the network.

NETWORK=testnet
MAGIC=(--testnet-magic 1097911063)
export CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file $PAYMENT_VKEY)
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Set the file name.

MARLOWE_FILE=test.marlowe


# Configure the contract.

CONTRACT_FILE=example.contract
STATE_FILE=test.state
DATUM_LOVELACE=3000000
REDEEM_MIN_SLOT=1000
REDEEM_MAX_SLOT=90000000

cat << EOI > $STATE_FILE
{
    "choices": [],
    "accounts": [
        [
            [
                {
                    "pk_hash": "$PUBKEYHASH_P"
                },
                {
                    "currency_symbol": "",
                    "token_name": ""
                }
            ],
            $DATUM_LOVELACE
        ]
    ],
    "minSlot": 10,
    "boundValues": []
}
EOI


# Create the contract, and extract the address, validator, datum hash, datum, and redeemer.

marlowe-cli export-marlowe "${MAGIC[@]}"                  \
                           --contract-file $CONTRACT_FILE \
                           --state-file $STATE_FILE       \
                           --out-file $MARLOWE_FILE       \
                           --print-stats

ADDRESS_S=$(jq -r '.validator.address' $MARLOWE_FILE)

PLUTUS_FILE=test.plutus
jq '.validator.script' $MARLOWE_FILE > $PLUTUS_FILE

DATUM_FILE=test.datum
jq '.datum.json' $MARLOWE_FILE > $DATUM_FILE

REDEEMER_FILE=test.redeemer
jq '.redeemer.json' $MARLOWE_FILE > $REDEEMER_FILE


# Find funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"

TX_0=8c604edc821e3795fb2547a0ad2b42367a42f46a0ddc98b160545159e1a8fd0c#0


# Fund the contract.

marlowe-cli transaction-create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --script-address "$ADDRESS_S"             \
                               --tx-out-datum-file $DATUM_FILE           \
                               --tx-out-value $DATUM_LOVELACE            \
                               --tx-in "$TX_0"                           \
                               --change-address "$ADDRESS_P"             \
                               --out-file tx.raw                         \
                               --required-signer $PAYMENT_SKEY           \
                               --submit=600


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

TX_1=f4561dfebda83edfd1bc3b34b3eaa2c7cf77eaf5dafdfac59a3d1ccea1cdcb86


# Redeem the contract.

marlowe-cli transaction-close "${MAGIC[@]}"                             \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --tx-in-script-file $PLUTUS_FILE          \
                              --tx-in-redeemer-file $REDEEMER_FILE      \
                              --tx-in-datum-file $DATUM_FILE            \
                              --tx-in-marlowe "$TX_1"#1                 \
                              --tx-in "$TX_1"#0                         \
                              --tx-in-collateral "$TX_1"#0              \
                              --tx-out "$ADDRESS_P"+$DATUM_LOVELACE     \
                              --change-address "$ADDRESS_P"             \
                              --invalid-before $REDEEM_MIN_SLOT         \
                              --invalid-hereafter $REDEEM_MAX_SLOT      \
                              --out-file tx.raw                         \
                              --required-signer $PAYMENT_SKEY           \
                              --submit=600


# See that the transaction succeeded: i.e., the 3 ADA should have been removed from the script address and transferred to the wallet address.

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"
