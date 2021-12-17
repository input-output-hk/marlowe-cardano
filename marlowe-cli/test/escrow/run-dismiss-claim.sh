#!/usr/bin/env bash


# Test of an escrow contract.


# The environment variable CARDANO_NODE_SOCKET_PATH must be set to the path to the cardano node's socket.
#
# The following tools must be on the path:
#   marlowe-cli
#   cardano-cli
#   sed
#   jq
#   xargs
#
# Signing and verification keys must be provided below for the "bystander" and "party" roles: to do this, set the
# environment variables "SELLER_PREFIX", "BUYER_PREFIX", and "MEDIATOR_PREFIX" where they appear below.


# This script exits with an error value if the end-to-end test fails.

set -e


# Select the network.

if false
then # Use the public testnet.
  MAGIC=(--testnet-magic 1097911063)
  SLOT_LENGTH=1000
  SLOT_OFFSET=1594369216000
else # Use the private testnet.
  MAGIC=(--testnet-magic 1564)
  SLOT_LENGTH=1000
  SLOT_OFFSET=1638215277000
fi


# The "seller" sells an item for a price.

SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
SELLER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$SELLER_PAYMENT_VKEY")
SELLER_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$SELLER_PAYMENT_VKEY")

echo "$SELLER_NAME is the seller."
echo "$SELLER_NAME's address: $SELLER_ADDRESS"
echo "$SELLER_NAME's public key hash: $SELLER_PUBKEYHASH"
echo "Contents of $SELLER_NAME's wallet:"
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"

TX_0_SELLER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$SELLER_ADDRESS"                  \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo
echo "$SELLER_NAME will spend the UTxO $TX_0_SELLER."


# The "buyer" buys an item for a price.

BUYER_PREFIX="$TREASURY/thomas-kyd"
BUYER_NAME="Thomas Kyd"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$BUYER_PAYMENT_VKEY")
BUYER_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$BUYER_PAYMENT_VKEY")

echo
echo "$BUYER_NAME is the buyer."
echo "$BUYER_NAME's address: $BUYER_ADDRESS"
echo "$BUYER_NAME's public key hash: $BUYER_PUBKEYHASH"
echo "Contents of $BUYER_NAME's wallet:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"

TX_0_BUYER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$BUYER_ADDRESS"                      \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo
echo "$BUYER_NAME will spend the UTxO $TX_0_BUYER."


# The "mediator" mediates any dispute between buyer and seller.

MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY")
MEDIATOR_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY")

echo
echo "$MEDIATOR_NAME is the mediator."
echo "$MEDIATOR_NAME's address: $MEDIATOR_ADDRESS"
echo "$MEDIATOR_NAME's public key hash: $MEDIATOR_PUBKEYHASH"
echo "Contents of $MEDIATOR_NAME's wallet:"
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"

TX_0_MEDIATOR=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$MEDIATOR_ADDRESS"                   \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo
echo "$MEDIATOR_NAME will spend the UTxO $TX_0_MEDIATOR."


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
                             --out-file escrow.plutus     \
                             --print-stats


# Find the current tip of the blockchain.

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
echo
echo "The tip is at slot $TIP."

echo
echo "The current POSIX time implies that the tip of the blockchain should be at slot $(($(date -u +%s) - $SLOT_OFFSET / $SLOT_LENGTH)). Tests may fail if this is not the case."


# The contract has a minimum slot and a timeout.

MINIMUM_SLOT="$TIP"

PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))

echo
echo "The current slot is $TIP."
echo "$BUYER_NAME must pay before slot $PAYMENT_DEADLINE."
echo "$BUYER_NAME has until slot $COMPAINT_DEADLINE to complain."
echo "$SELLER_NAME has until slot $DISPUTE_DEADLINE to dispute a complaint."
echo "$MEDIATOR_NAME has until slot $MEDIATION_DEADLINE to decide on a disputed complaint."

MINIMUM_ADA=3000000
PRICE=256000000
echo
echo "The selling price is $PRICE lovelace."


# Transaction 1. Create the contract by providing the minimum ADA.

echo
echo "Transaction 1. Create the contract by providing the minimum ADA."

marlowe-cli contract-escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "PK=$SELLER_PUBKEYHASH"           \
                            --buyer "PK=$BUYER_PUBKEYHASH"             \
                            --mediator "PK=$MEDIATOR_PUBKEYHASH"       \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-file tx-1.marlowe

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
marlowe-cli transaction-create "${MAGIC[@]}"                              \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX_0_MEDIATOR"                   \
                               --change-address "$MEDIATOR_ADDRESS"       \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"       \
                               --tx-out-datum-file tx-1.datum             \
                               --tx-out-marlowe "$MINIMUM_ADA"            \
                               --out-file tx-1.raw                        \
                               --print-stats                              \
                               --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from $MEDIATOR_NAME in the transaction $TX_1."

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo
echo "Here is the UTxO at $MEDIATOR_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"


# Transaction 2. Buyer deposits funds into seller's account.

echo
echo "Transaction 2. Buyer deposits funds into seller's account."

marlowe-cli input-deposit --deposit-account "PK=$SELLER_PUBKEYHASH" \
                          --deposit-party "PK=$BUYER_PUBKEYHASH"    \
                          --deposit-amount "$PRICE"                 \
                          --out-file "tx-2.input"

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
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_1"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-1.datum             \
                                --tx-in-redeemer-file tx-2.redeemer       \
                                --tx-in "$TX_0_BUYER"                     \
                                --tx-in-collateral "$TX_0_BUYER"          \
                                --required-signer "$BUYER_PAYMENT_SKEY"   \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-2.datum            \
                                --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA"    \
                                --change-address "$BUYER_ADDRESS"         \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-2.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The contract received the deposit of $PRICE lovelace from $BUYER_NAME in the transaction $TX_2"

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo
echo "Here is the UTxO at $BUYER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"


# Transaction 3. The buyer reports that there is a problem.

echo
echo "Transaction 3. The buyer reports that there is a problem."

marlowe-cli input-choose --choice-name "Report problem"        \
                         --choice-party "PK=$BUYER_PUBKEYHASH" \
                         --choice-number 1                     \
                         --out-file tx-3.input

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

echo
echo "Submit the transaction."
TX_3=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_2"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-2.datum             \
                                --tx-in-redeemer-file tx-3.redeemer       \
                                --tx-in "$TX_2"#0                         \
                                --tx-in-collateral "$TX_2"#0              \
                                --required-signer "$BUYER_PAYMENT_SKEY"   \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-3.datum            \
                                --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA"    \
                                --change-address "$BUYER_ADDRESS"         \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-3.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The reporting of a problem was recorded in the transaction $TX_3."

echo
echo "There is no UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo
echo "Here is the UTxO at $BUYER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"


# Transaction 4. The seller disputes that there is a problem.

echo
echo "Transaction 4. The seller disputes that there is a problem."

marlowe-cli input-choose --choice-name "Dispute problem"        \
                         --choice-party "PK=$SELLER_PUBKEYHASH" \
                         --choice-number 0                      \
                         --out-file tx-4.input

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
echo "Construct the datum. Here is its hash:"
marlowe-cli export-datum --contract-file tx-4.contract \
                         --state-file    tx-4.state    \
                         --out-file      tx-4.datum    \
                         --print-stats

echo
echo "Submit the transaction."
TX_4=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_3"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-3.datum             \
                                --tx-in-redeemer-file tx-4.redeemer       \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-4.datum            \
                                --tx-in "$TX_0_SELLER"                    \
                                --tx-in-collateral "$TX_0_SELLER"         \
                                --required-signer "$SELLER_PAYMENT_SKEY"  \
                                --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA"   \
                                --change-address "$SELLER_ADDRESS"        \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-4.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The dispute that this is a problem is recorded in the transaction $TX_4."

echo
echo "Here is the UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo
echo "Here is the UTxO at $SELLER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"


# Transaction 5. The mediator confirms the claim.

echo
echo "Transaction 5. The mediator confirms the claim."

marlowe-cli input-choose --choice-name "Confirm claim"            \
                         --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                         --choice-number 1                        \
                         --out-file tx-5.input

echo
echo "Compute the transaction."
marlowe-cli compute --contract-file tx-4.contract          \
                    --state-file    tx-4.state             \
                    --input-file    tx-5.input             \
                    --out-file      tx-5.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats

jq '.state'    tx-5.marlowe > tx-5.state
jq '.contract' tx-5.marlowe > tx-5.contract

echo
echo "Construct the redeemer."
marlowe-cli export-redeemer --input-file tx-5.input    \
                            --out-file   tx-5.redeemer \
                            --print-stats

echo
echo "Submit the transaction."
TX_5=$(
marlowe-cli transaction-close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_4"#1                  \
                              --tx-in-script-file escrow.plutus          \
                              --tx-in-datum-file tx-4.datum              \
                              --tx-in-redeemer-file tx-5.redeemer        \
                              --tx-in "$TX_1"#0                          \
                              --tx-in-collateral "$TX_1"#0               \
                              --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                              --tx-out "$BUYER_ADDRESS+$PRICE"           \
                              --change-address "$MEDIATOR_ADDRESS"       \
                              --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA"  \
                              --invalid-before "$TIP"                    \
                              --invalid-hereafter "$(($TIP+4*3600))"     \
                              --out-file tx-5.raw                        \
                              --print-stats                              \
                              --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo
echo "The confirmation of the claim resulted in closing the contract, paying $PRICE lovelace to $BUYER_NAME and $MINIMUM_ADA lovelace to $MEDIATOR_NAME in the transaction $TX_5."

echo
echo "There is no UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo
echo "Here is the UTxO at $BUYER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo
echo "Here is the UTxO at $MEDIATOR_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"


# Clean up.

echo
echo "Clean up."

cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" --out-file /dev/stdout  \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$SELLER_ADDRESS"        \
                                               --out-file tx-6.raw                       \
                                               --required-signer "$SELLER_PAYMENT_SKEY"  \
                                               --print-stats                             \
                                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'

echo
echo "Here is the UTxO at $SELLER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"

cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" --out-file /dev/stdout   \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$BUYER_ADDRESS"         \
                                               --out-file tx-7.raw                       \
                                               --required-signer "$BUYER_PAYMENT_SKEY"   \
                                               --print-stats                             \
                                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'

echo
echo "Here is the UTxO at $BUYER_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"

cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                            \
| sed -e 's/"//g;s/^/--tx-in /'                                                           \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                              \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                                               --change-address "$MEDIATOR_ADDRESS"       \
                                               --out-file tx-8.raw                        \
                                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                                               --print-stats                              \
                                               --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'

echo
echo "Here is the UTxO at $MEDIATOR_NAME's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"
