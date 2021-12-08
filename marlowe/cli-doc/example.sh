#!/usr/bin/env bash


####
#### Example for using `marlowe-cli` to run Marlowe contracts on `testnet`.
####
#### This uses the `address`, `validator`, `data`, `redeemer` commands of `cardano-cli`.
####


# Make sure that cardano-cli is on the path!


# Select the network.

NETWORK=testnet
MAGIC=(--testnet-magic 1097911063)
export CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file $PAYMENT_VKEY)
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Set the file names.

PLUTUS_FILE=test.plutus
DATUM_FILE=test.datum
REDEEMER_FILE=test.redeemer


# Configure the contract.

CONTRACT_FILE=example.contract
STATE_FILE=test.state
DATUM_LOVELACE=3000000
REDEEM_MIN_SLOT=1000
REDEEM_MAX_SLOT=50000000

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


# Create the contract.

ADDRESS_S=$(marlowe-cli address "${MAGIC[@]}")

marlowe-cli validator "${MAGIC[@]}" --out-file $PLUTUS_FILE
marlowe-cli datum --contract-file $CONTRACT_FILE \
                  --state-file $STATE_FILE       \
                  --out-file $DATUM_FILE

marlowe-cli redeemer --out-file $REDEEMER_FILE


# Find funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"

TX_0=1e3b073ad8cca27ba5a81d3a52b1b8cf1ea0770e57a5ae4b10448f029b0ecf2a#0


# Fund the contract.

marlowe-cli create "${MAGIC[@]}"                             \
                   --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                   --script-address "$ADDRESS_S"             \
                   --tx-out-datum-file $DATUM_FILE           \
                   --tx-out-value $DATUM_LOVELACE            \
                   --tx-in "$TX_0"                           \
                   --change-address "$ADDRESS_P"             \
                   --out-file tx.raw

marlowe-cli submit "${MAGIC[@]}"                             \
                   --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                   --tx-body-file tx.raw                     \
                   --required-signer $PAYMENT_SKEY


# Find the funding transaction, and enter its UTxO as "TX_1".

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

TX_1=554b41253b6613cd75a32cd7521599528de82a37dc091546ba54ab7eff289279


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
                  --out-file tx.raw

marlowe-cli submit "${MAGIC[@]}"                             \
                   --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                   --tx-body-file tx.raw                     \
                   --required-signer $PAYMENT_SKEY


# See that the transaction succeeded: i.e., the 3 ADA should have been removed from the script address and transferred to the wallet address.

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"

#### Voilà! See <https://testnet.cardanoscan.io/transaction/bcb0f4cd7d55fe08b01ffa797577128093ff82dd549faa1e5ef8487f84a215ac>.
