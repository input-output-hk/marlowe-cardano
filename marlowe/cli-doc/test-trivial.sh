#!/usr/bin/env bash


# Test of a trivial contract.


# The environment variable CARDANO_NODE_SOCKET_PATH must be set to the path to the cardano node's socket.
#
# The environment variable TREASURY must be set to the location of the payment and signing keys for the parties involved.
#
# The following tools must be on the path:
#   marlowe-cli
#   cardano-cli
#   sed
#   jq


# This script exits with an error value if the end-to-end test fails.

echo
echo "Test of a trivial contract."

set -e


# Select the network.

####NETWORK=private
####MAGIC=(--testnet-magic 1564)

MAGIC=(--testnet-magic 1097911063)
SLOT_LENGTH=1000
SLOT_OFFSET=1594369216000


# The "bystander" simply provides the minimum ada to be held in the contract while it is active.

BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_NAME="Christopher Marlowe"
BYSTANDER_PAYMENT_SKEY="$BYSTANDER_PREFIX".skey
BYSTANDER_PAYMENT_VKEY="$BYSTANDER_PREFIX".vkey
BYSTANDER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY")
BYSTANDER_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY")

echo
echo "$BYSTANDER_NAME is the minimum-ADA provider."
echo "$BYSTANDER_NAME's address: $BYSTANDER_ADDRESS"
echo "$BYSTANDER_NAME's public key hash: $BYSTANDER_PUBKEYHASH"
echo "Contents of $BYSTANDER_NAME's wallet:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS"

TX_0_BYSTANDER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$BYSTANDER_ADDRESS"                  \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo
echo "$BYSTANDER_NAME will spend the UTxO $TX_0_BYSTANDER."


# The "party" deposits and removes funds from the contract.

PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey
PARTY_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_PAYMENT_VKEY")
PARTY_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$PARTY_PAYMENT_VKEY")

echo
echo "$PARTY_NAME's address: $PARTY_ADDRESS"
echo "$PARTY_NAME's public key hash: $PARTY_PUBKEYHASH"
echo "Contents of $PARTY_NAME's wallet:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS"

TX_0_PARTY=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$PARTY_ADDRESS"                      \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key'
)
echo
echo "$PARTY_NAME will spend the UTxO $TX_0_PARTY."


# Compute the contract address and validator script.

echo
echo "Compute the contract address and validator script."

CONTRACT_ADDRESS=$(
marlowe-cli export-address "${MAGIC[@]}" \
            --slot-length "$SLOT_LENGTH" \
            --slot-offset "$SLOT_OFFSET" \
)

marlowe-cli export-validator "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
                             --out-file trivial.plutus    \
                             --print-stats


# Find the current tip of the blockchain.

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
echo
echo "The tip is at slot $TIP."


# The contract has a minimum slot and a timeout.

MINIMUM_SLOT="$TIP"
TIMEOUT_SLOT="$(($TIP+10*24*3600))"


# Configure the contract.

MINIMUM_ADA=3000000
DEPOSIT_LOVELACE=12000000
WITHDRAWAL_LOVELACE=5000000
CLOSE_LOVELACE=$(($DEPOSIT_LOVELACE-$WITHDRAWAL_LOVELACE))
ARBITRARY_LOVELACE=4000000

echo
echo "$BYSTANDER_NAME will provide $MINIMUM_ADA lovelace during the contract's operation, so that it satisfies the minimmum-ADA requirement."
echo "$PARTY_NAME will deposit $DEPOSIT_LOVELACE lovelace."
echo "$PARTY_NAME will wait until notified that they can withdraw $WITHDRAWAL_LOVELACE lovelace."
echo "After another notification, $PARTY_NAME will withdrawn the remaining $CLOSE_LOVELACE lovelace and $BYSTANDER_NAME will receive his $MINIMUM_ADA lovelace back."

marlowe-cli contract-trivial --bystander "PK=$BYSTANDER_PUBKEYHASH"       \
                             --minimum-ada "$MINIMUM_ADA"                 \
                             --minimum-slot "$MINIMUM_SLOT"               \
                             --party "PK=$PARTY_PUBKEYHASH"               \
                             --deposit-lovelace "$DEPOSIT_LOVELACE"       \
                             --withdrawal-lovelace "$WITHDRAWAL_LOVELACE" \
                             --timeout "$TIMEOUT_SLOT"                    \
                             --out-file tx-1.marlowe


# Transaction 1. Create the contract by providing the minimum ADA.

echo
echo "Transaction 1. Create the contract by providing the minimum ADA."

jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract

echo
echo "Construct the datum. Here is its hash:"
marlowe-cli export-datum --contract-file tx-1.contract \
                         --state-file    tx-1.state    \
                         --out-file      tx-1.datum    \
                         --print-stats

echo
echo "Submit the transaction."
TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                               --tx-in "$TX_0_BYSTANDER"                   \
                               --change-address "$BYSTANDER_ADDRESS"       \
                               --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"        \
                               --tx-out-datum-file tx-1.datum              \
                               --tx-out-marlowe "$MINIMUM_ADA"             \
                               --out-file tx-1.raw                         \
                               --print-stats                               \
                               --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace the UTxO $TX_1."

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo
echo "Here is the UTxO at $BYSTANDER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"


# Transaction 2. Make the initial deposit.

echo
echo "Transaction 2. Make the initial deposit."

marlowe-cli input-deposit --deposit-account "PK=$PARTY_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_PUBKEYHASH"   \
                          --deposit-amount "$DEPOSIT_LOVELACE"     \
                          --out-file tx-2.input

echo
echo "Compute the transaction."
marlowe-cli compute --contract-file tx-1.contract          \
                    --state-file    tx-1.state             \
                    --input-file    tx-2.input             \
                    --out-file      tx-2.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats

jq '.state'    tx-2.marlowe > tx-2.state
jq '.contract' tx-2.marlowe > tx-2.contract

echo
echo "Construct the redeemer."
marlowe-cli export-redeemer --input-file tx-2.input    \
                            --out-file   tx-2.redeemer \
                            --print-stats

echo
echo "Construct the datum. Here is its hash:"
marlowe-cli export-datum --contract-file tx-2.contract \
                         --state-file    tx-2.state    \
                         --out-file      tx-2.datum    \
                         --print-stats

CONTRACT_VALUE_2=$(jq '.accounts | [.[][1]] | add' tx-2.state)

echo
echo "Submit the transaction."
TX_2=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                                 \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH"     \
                                --script-address "$CONTRACT_ADDRESS"          \
                                --tx-in-marlowe "$TX_1"#1                     \
                                --tx-in-script-file trivial.plutus            \
                                --tx-in-datum-file tx-1.datum                 \
                                --tx-in-redeemer-file tx-2.redeemer           \
                                --tx-in "$TX_0_PARTY"                         \
                                --tx-in-collateral "$TX_0_PARTY"              \
                                --required-signer "$PARTY_PAYMENT_SKEY"       \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"          \
                                --tx-out-datum-file tx-2.datum                \
                                --tx-out "$PARTY_ADDRESS+$ARBITRARY_LOVELACE" \
                                --change-address "$PARTY_ADDRESS"             \
                                --invalid-before "$TIP"                       \
                                --invalid-hereafter "$(($TIP+4*3600))"        \
                                --out-file tx-2.raw                           \
                                --print-stats                                 \
                                --submit=600                                  \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The contract received the deposit of $DEPOSIT_LOVELACE lovelace in the UTxO $TX_2"

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo
echo "Here is the UTxO at $PARTY_NAME address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"


# Transaction 3. Make the first withdrawal.

echo
echo "Transaction 3. Make the first withdrawal."

marlowe-cli input-notify --out-file "tx-3.input"

echo
echo "Compute the transaction."
marlowe-cli compute --contract-file tx-2.contract          \
                    --state-file    tx-2.state             \
                    --input-file    tx-3.input             \
                    --out-file      tx-3.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats

jq '.state'    tx-3.marlowe > tx-3.state
jq '.contract' tx-3.marlowe > tx-3.contract

echo
echo "Construct the redeemer."
marlowe-cli export-redeemer --input-file tx-3.input    \
                            --out-file   tx-3.redeemer \
                            --print-stats

echo
echo "Construct the datum. Here is its hash:"
marlowe-cli export-datum --contract-file tx-3.contract \
                         --state-file    tx-3.state    \
                         --out-file      tx-3.datum    \
                         --print-stats

CONTRACT_VALUE_3=$(jq '.accounts | [.[][1]] | add' tx-3.state)

echo
echo "Submit the transaction."
TX_3=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                                  \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                                --script-address "$CONTRACT_ADDRESS"           \
                                --tx-in-marlowe "$TX_2"#1                      \
                                --tx-in-script-file trivial.plutus             \
                                --tx-in-datum-file tx-2.datum                  \
                                --tx-in-redeemer-file tx-3.redeemer            \
                                --tx-in "$TX_2"#0                              \
                                --tx-in-collateral "$TX_2"#0                   \
                                --required-signer "$PARTY_PAYMENT_SKEY"        \
                                --tx-out-marlowe "$CONTRACT_VALUE_3"           \
                                --tx-out-datum-file tx-3.datum                 \
                                --tx-out "$PARTY_ADDRESS+$WITHDRAWAL_LOVELACE" \
                                --change-address "$PARTY_ADDRESS"              \
                                --invalid-before "$(($TIP))"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"         \
                                --out-file tx-3.raw                            \
                                --print-stats                                  \
                                --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The contract made a payment of $WITHDRAWAL_LOVELACE lovelace to $PARTY_NAME in the UTxO $TX_3."

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo
echo "Here is the UTxO at $PARTY_NAME address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"


# Transaction 4. Close the contract.

echo
echo "Transaction 4. Close the contract."

marlowe-cli input-notify --out-file "tx-4.input"

echo
echo "Compute the transaction."
marlowe-cli compute --contract-file tx-3.contract          \
                    --state-file    tx-3.state             \
                    --input-file    tx-4.input             \
                    --out-file      tx-4.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats

jq '.state'    tx-4.marlowe > tx-4.state
jq '.contract' tx-4.marlowe > tx-4.contract

echo
echo "Construct the redeemer."
marlowe-cli export-redeemer --input-file tx-4.input    \
                            --out-file   tx-4.redeemer \
                            --print-stats

echo
echo "Submit the transaction."
TX_4=$(
marlowe-cli transaction-close "${MAGIC[@]}"                               \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                              --tx-in-marlowe "$TX_3"#1                   \
                              --tx-in-script-file trivial.plutus          \
                              --tx-in-datum-file tx-3.datum               \
                              --tx-in-redeemer-file tx-4.redeemer         \
                              --tx-in "$TX_3"#0                           \
                              --tx-in-collateral "$TX_3"#0                \
                              --required-signer "$PARTY_PAYMENT_SKEY"     \
                              --tx-out "$PARTY_ADDRESS+$CLOSE_LOVELACE"   \
                              --change-address "$PARTY_ADDRESS"           \
                              --tx-out "$BYSTANDER_ADDRESS+$MINIMUM_ADA"  \
                              --invalid-before "$TIP"                     \
                              --invalid-hereafter "$(($TIP+4*3600))"      \
                              --out-file tx-4.raw                         \
                              --print-stats                               \
                              --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The closing of the contract paid $CLOSE_LOVELACE lovelace to $PARTY_NAME and $MINIMUM_ADA lovelace to $BYSTANDER_NAME in the UTxO $TX_4."

echo
echo "There is no UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo
echo "Here is the UTxO at $PARTY_NAME address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo
echo "Here is the UTxO at $BYSTANDER_NAME address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
