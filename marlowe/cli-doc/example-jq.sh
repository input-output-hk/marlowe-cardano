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

marlowe-cli export "${MAGIC[@]}"                  \
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

TX_0=01c12968be91e7d96d3b223e3fd6fea330155f43ad81d7de89db9396766403ce#0


# Fund the contract.

marlowe-cli create "${MAGIC[@]}"                             \
                   --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                   --script-address "$ADDRESS_S"             \
                   --tx-out-datum-file $DATUM_FILE           \
                   --tx-out-value $DATUM_LOVELACE            \
                   --tx-in "$TX_0"                           \
                   --change-address "$ADDRESS_P"             \
                   --out-file tx.raw                         \
                   --required-signer $PAYMENT_SKEY           \
                   --submit


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

TX_1=8c3da8804d8ef034a1f1e4a2aee7cae42c6b3d1cfb0aabe85c68e9b3970b10fd


# Redeem the contract.

marlowe-cli close "${MAGIC[@]}"                             \
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
                  --submit


# See that the transaction succeeded: i.e., the 3 ADA should have been removed from the script address and transferred to the wallet address.

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"
