#!/usr/bin/env bash


# Simple end-to-end test of `marlowe-cli`.
#
#
# The environment variable CARDANO_NODE_SOCKET_PATH must be set.
#
# The following tools must be on the path:
#   marlowe-cli
#   cardano-cli
#   sed
#   jq
#   timeout
#
# This script exits with an error value if the end-to-end test fails.


set -ex


# Select the network.

NETWORK=testnet
MAGIC=(--testnet-magic 1097911063)

if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
then
  export CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket
fi


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file $PAYMENT_VKEY)
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Find the contract address.

ADDRESS_S=$(marlowe-cli export-address "${MAGIC[@]}")
echo "$ADDRESS_S"


# Create the Plutus script for the validator.

marlowe-cli export-validator "${MAGIC[@]}" --out-file example.plutus


# Generate the example contract, state, and inputs files for each step.

marlowe-cli example "$PUBKEYHASH_P" --write-files > /dev/null

for i in 0 1 2
do
  marlowe-cli export-datum --contract-file example-$i.contract \
                           --state-file    example-$i.state    \
                           --out-file      example-$i.datum
done

for i in 0 1
do
  marlowe-cli export-redeemer --out-file example-$i.redeemer
done
marlowe-cli export-redeemer --input-file example-2.input \
                            --out-file   example-2.redeemer


# 0. Find some funds, and enter the selected UTxO as "TX_0".

echo -e \\nFIND THE FUNDING UTXO\\n

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"

TX_0=$(
cardano-cli query utxo "${MAGIC[@]}"                                        \
                       --address "$ADDRESS_P"                               \
                       --out-file /dev/stdout                               \
| jq '. | to_entries[] | select(.value.value.lovelace >= 120000000) | .key' \
| sed -n -e '1{s/"//g;p}'
)
echo TxId '"'"$TX_0"'"'


# Check the existing UTxOs at the script address.

echo -e \\nEXISTING UTXOS AT SCRIPT\\n

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"


# Fund the contract by sending the initial funds and setting the initial state.

echo -e \\nCREATE THE CONTRACT\\n

TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --script-address "$ADDRESS_S"             \
                               --tx-out-datum-file example-2.datum       \
                               --tx-out-value 3000000                    \
                               --tx-in "$TX_0"                           \
                               --change-address "$ADDRESS_P"             \
                               --out-file tx.raw                         \
                               --required-signer $PAYMENT_SKEY           \
                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId "$TX_1"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"


# 1. Deposit 10 ADA.

echo -e \\nDEPOSIT 10 ADA\\n

TX_2=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$ADDRESS_S"             \
                                --tx-in-script-file example.plutus        \
                                --tx-in-redeemer-file example-2.redeemer  \
                                --tx-in-datum-file example-2.datum        \
                                --required-signer $PAYMENT_SKEY           \
                                --tx-in-marlowe "$TX_1"#1                 \
                                --tx-in "$TX_1"#0                         \
                                --tx-in-collateral "$TX_1"#0              \
                                --tx-out-datum-file example-1.datum       \
                                --tx-out-value 13000000                   \
                                --tx-out "$ADDRESS_P"+50000000            \
                                --change-address "$ADDRESS_P"             \
                                --invalid-before    40000000              \
                                --invalid-hereafter 80000000              \
                                --out-file tx.raw                         \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId "$TX_2"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"


## 2. Pay 5 ADA back.

echo -e \\nPAY 5 ADA BACK\\n

TX_3=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$ADDRESS_S"             \
                                --tx-in-script-file example.plutus        \
                                --tx-in-redeemer-file example-1.redeemer  \
                                --tx-in-datum-file example-1.datum        \
                                --required-signer $PAYMENT_SKEY           \
                                --tx-in-marlowe "$TX_2"#1                 \
                                --tx-in "$TX_2"#0                         \
                                --tx-in-collateral "$TX_2"#0              \
                                --tx-out-datum-file example-0.datum       \
                                --tx-out-value 8000000                    \
                                --tx-out "$ADDRESS_P"+50000000            \
                                --change-address "$ADDRESS_P"             \
                                --invalid-before    40000000              \
                                --invalid-hereafter 80000000              \
                                --out-file tx.raw                         \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId "$TX_3"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"


# 3. Withdrawn the remaining 8 ADA.

echo -e \\nWITHDRAW THE REMAINING 8 ADA\\n

TX_4=$(
marlowe-cli transaction-close "${MAGIC[@]}"                             \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --tx-in-script-file example.plutus        \
                              --tx-in-redeemer-file example-0.redeemer  \
                              --tx-in-datum-file example-0.datum        \
                              --tx-in-marlowe "$TX_3"#1                 \
                              --tx-in "$TX_3"#0                         \
                              --tx-in-collateral "$TX_3"#0              \
                              --tx-out "$ADDRESS_P"+8000000             \
                              --change-address "$ADDRESS_P"             \
                              --invalid-before    40000000              \
                              --invalid-hereafter 80000000              \
                              --out-file tx.raw                         \
                              --required-signer $PAYMENT_SKEY           \
                              --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId "$TX_4"


# See that the transaction succeeded.

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"

echo -e \\nSUCCESS\\n


# Clean up.

echo -e \\nCLEAN UP\\n

TX_5=$(
cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                     \
| sed -e 's/"//g;s/^/--tx-in /'                                                    \
| xargs marlowe-cli transaction-simple "${MAGIC[@]}"                               \
                                       --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                                       --change-address "$ADDRESS_P"               \
                                       --out-file tx.raw                           \
                                       --required-signer $PAYMENT_SKEY             \
                                       --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId "$TX_5"

cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"
