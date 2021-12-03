#!/usr/bin/env bash


# Simple end-to-end test of `marlowe-cli`.

set -ex


# Select the network.

NETWORK=testnet
MAGIC="--testnet-magic 1097911063"
CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket


# Select the wallet.

PAYMENT_SKEY=payment.skey
PAYMENT_VKEY=payment.vkey
ADDRESS_P=$(cardano-cli address build $MAGIC --payment-verification-key-file $PAYMENT_VKEY)
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


# Find the contract address.

ADDRESS_S=$(marlowe-cli address $MAGIC)
echo $ADDRESS_S


# Create the Plutus script for the validator.

marlowe-cli validator $MAGIC --out-file example.plutus


# Generate the example contract, state, and inputs files for each step.

marlowe-cli example --write-files > /dev/null
for i in 0 1 2
do
  marlowe-cli datum    --contract-file example-$i.contract \
                       --state-file    example-$i.state    \
                       --out-file      example-$i.datum
  marlowe-cli redeemer --inputs-file   example-$i.inputs   \
                       --out-file      example-$i.redeemer
done
sed -i -e s/d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657/$PUBKEYHASH_P/ example-[012].{contract,state,inputs,datum,redeemer}


# 0. Find some funds, and enter the selected UTxO as "TX_0".

cardano-cli query utxo $MAGIC --address $ADDRESS_P

#### FIXME: Discover this Tx automatically.

TX_0=f878ffba37026081afba2408db8a534b29a27d2eb9535e8b0666d70b9967c455


# Fund the contract by sending the initial funds and setting the initial state.

TX_1=$(
marlowe-cli create $MAGIC                                  \
                   --socket-path $CARDANO_NODE_SOCKET_PATH \
                   --script-address $ADDRESS_S             \
                   --tx-out-datum-file example-2.datum     \
                   --tx-out-value 3000000                  \
                   --tx-in $TX_0#0                         \
                   --change-address $ADDRESS_P             \
                   --out-file tx.raw                       \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId $TX_1

marlowe-cli submit $MAGIC                                  \
                   --socket-path $CARDANO_NODE_SOCKET_PATH \
                   --required-signer $PAYMENT_SKEY         \
                   --tx-body-file tx.raw


# Wait until the transaction is appears on the blockchain.

#### FIXME: Automatically wait.

cardano-cli query utxo $MAGIC --address $ADDRESS_S


# 1. Deposit 10 ADA.

TX_2=$(
marlowe-cli advance $MAGIC                                   \
                    --socket-path $CARDANO_NODE_SOCKET_PATH  \
                    --script-address $ADDRESS_S              \
                    --tx-in-script-file example.plutus       \
                    --tx-in-redeemer-file example-2.redeemer \
                    --tx-in-datum-file example-2.datum       \
                    --required-signer $PAYMENT_SKEY          \
                    --tx-in-marlowe $TX_1#1                  \
                    --tx-in $TX_1#0                          \
                    --tx-in-collateral $TX_1#0               \
                    --tx-out-datum-file example-1.datum      \
                    --tx-out-value 13000000                  \
                    --tx-out $ADDRESS_P+50000000             \
                    --change-address $ADDRESS_P              \
                    --invalid-before $REDEEM_MIN_SLOT        \
                    --invalid-hereafter $REDEEM_MAX_SLOT     \
                    --out-file tx.raw                        \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId $TX_2

marlowe-cli submit $MAGIC                                  \
                   --socket-path $CARDANO_NODE_SOCKET_PATH \
                   --required-signer $PAYMENT_SKEY         \
                   --tx-body-file tx.raw


# Wait until the transaction is appears on the blockchain.

#### FIXME: Automatically wait.

cardano-cli query utxo $MAGIC --address $ADDRESS_S


## 2. Pay 5 ADA back.

TX_3=$(
marlowe-cli advance $MAGIC                                   \
                    --socket-path $CARDANO_NODE_SOCKET_PATH  \
                    --script-address $ADDRESS_S              \
                    --tx-in-script-file example.plutus       \
                    --tx-in-redeemer-file example-1.redeemer \
                    --tx-in-datum-file example-1.datum       \
                    --required-signer $PAYMENT_SKEY          \
                    --tx-in-marlowe $TX_2#1                  \
                    --tx-in $TX_2#0                          \
                    --tx-in-collateral $TX_2#0               \
                    --tx-out-datum-file example-0.datum      \
                    --tx-out-value 8000000                   \
                    --tx-out $ADDRESS_P+50000000             \
                    --change-address $ADDRESS_P              \
                    --invalid-before $REDEEM_MIN_SLOT        \
                    --invalid-hereafter $REDEEM_MAX_SLOT     \
                    --out-file tx.raw                        \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId $TX_3

marlowe-cli submit $MAGIC                                  \
                   --socket-path $CARDANO_NODE_SOCKET_PATH \
                   --required-signer $PAYMENT_SKEY         \
                   --tx-body-file tx.raw


# 3. Withdrawn the remaining 8 ADA.

#### FIXME: Automatically wait.

cardano-cli query utxo $MAGIC --address $ADDRESS_S

TX_4=$(
marlowe-cli close $MAGIC                                  \
                  --socket-path $CARDANO_NODE_SOCKET_PATH \
                  --tx-in-script-file example.plutus      \
                  --tx-in-redeemer-file example-0.redeemer\
                  --tx-in-datum-file example-0.datum      \
                  --tx-in-marlowe $TX_3#1                 \
                  --tx-in $TX_3#0                         \
                  --tx-in-collateral $TX_3#0              \
                  --tx-out $ADDRESS_P+8000000             \
                  --change-address $ADDRESS_P             \
                  --invalid-before $REDEEM_MIN_SLOT       \
                  --invalid-hereafter $REDEEM_MAX_SLOT    \
                  --out-file tx.raw                       \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo TxId $TX_4

marlowe-cli submit $MAGIC                                  \
                   --socket-path $CARDANO_NODE_SOCKET_PATH \
                   --required-signer $PAYMENT_SKEY         \
                   --tx-body-file tx.raw


# See that the transaction succeeded.

#### FIXME: Automatically wait.

cardano-cli query utxo $MAGIC --address $ADDRESS_S

cardano-cli query utxo $MAGIC --address $ADDRESS_P

