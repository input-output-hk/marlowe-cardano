#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -e

echo '# Example Escrow Contract: "Dismiss Claim"'

echo "In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, and the mediator dismisses the buyer's claim."
echo
echo '![Flow chart for "dismiss claim".](dismiss-claim.svg)'

echo "## Prerequisites"

echo "The environment variable "'`CARDANO_NODE_SOCKET_PATH`'" must be set to the path to the cardano node's socket."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo
echo 'Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `PARTY_PREFIX` where they appear below.'

echo "## Preliminaries"

echo "### Select Network"

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

echo "### Select Parties"

echo "#### The Seller"

echo "The seller sells an item for a price."

SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
SELLER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$SELLER_PAYMENT_VKEY" \
)
SELLER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$SELLER_PAYMENT_VKEY"
)

echo "The seller $SELLER_NAME has the address "'`'"$SELLER_ADDRESS"'`'" and public-key hash "'`'"$SELLER_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"

echo "We select the tokenless UTxO with the most funds to use in executing the contract."

TX_0_SELLER=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$SELLER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)

echo "$SELLER_NAME will spend the UTxO "'`'"$TX_0_SELLER"'`.'

echo "### The Buyer"

BUYER_PREFIX="$TREASURY/thomas-kyd"
BUYER_NAME="Thomas Kyd"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$BUYER_PAYMENT_VKEY" \
)
BUYER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BUYER_PAYMENT_VKEY"
)

echo "The buyer $BUYER_NAME has the address "'`'"$BUYER_ADDRESS"'`'" and public-key hash "'`'"$BUYER_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"

echo "We select the tokenless UTxO with the most funds to use in executing the contract."

TX_0_BUYER=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BUYER_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)

echo "$BUYER_NAME will spend the UTxO "'`'"$TX_0_BUYER"'`.'

echo "### The Mediator"

MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY" \
)
MEDIATOR_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY"
)

echo "The mediator $MEDIATOR_NAME has the address "'`'"$MEDIATOR_ADDRESS"'`'" and public-key hash "'`'"$MEDIATOR_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"

echo "We select the UTxO with the most funds to use in executing the contract."

TX_0_MEDIATOR=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$MEDIATOR_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)

echo "$MEDIATOR_NAME will spend the UTxO "'`'"$TX_0_MEDIATOR"'`.'

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(($(date -u +%s) - $SLOT_OFFSET / $SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

echo "The contract has a minimum slot and several deadlines."

MINIMUM_SLOT="$TIP"

PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))

echo "* The current slot is $TIP."
echo "* The buyer $BUYER_NAME must pay before slot $PAYMENT_DEADLINE."
echo "* They buyer $BUYER_NAME has until slot $COMPLAINT_DEADLINE to complain."
echo "* The seller $SELLER_NAME has until slot $DISPUTE_DEADLINE to dispute a complaint."
echo "* The mediator $MEDIATOR_NAME has until slot $MEDIATION_DEADLINE to decide on a disputed complaint."

echo "The contract also involves the price of the good exchanged and a minimum-ADA value."

MINIMUM_ADA=3000000
PRICE=256000000

echo "The selling price is $PRICE lovelace."

echo "We create the contract for the previously specified parameters."

marlowe-cli template escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "PK=$SELLER_PUBKEYHASH"           \
                            --buyer "PK=$BUYER_PUBKEYHASH"             \
                            --mediator "PK=$MEDIATOR_PUBKEYHASH"       \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-contract-file tx-1.contract          \
                            --out-state-file    tx-1.state

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo 'First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.'

marlowe-cli run initialize "${MAGIC[@]}"                 \
                           --slot-length "$SLOT_LENGTH"  \
                           --slot-offset "$SLOT_OFFSET"  \
                           --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.marlowe  \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`.marlowe`'" file."

CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "The mediator $MEDIATOR_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`--print-stats`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR"                   \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe            \
                        --out-file tx-1.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the mediator $MEDIATOR_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here is the UTxO at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Buyer Deposits Funds into Seller's Account."

echo "First we compute the Marlowe input required to make the initial deposit by the buyer."

marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "PK=$SELLER_PUBKEYHASH" \
                        --deposit-party "PK=$BUYER_PUBKEYHASH"    \
                        --deposit-amount "$PRICE"                 \
                        --invalid-before "$TIP"                   \
                        --invalid-hereafter "$(($TIP+4*3600))"    \
                        --out-file tx-2.marlowe                   \
                        --print-stats

echo "Now the buyer $BUYER_NAME submits the transaction along with their deposit:"

TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_0_BUYER"          \
                        --tx-in "$TX_0_BUYER"                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$BUYER_ADDRESS"         \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the deposit of $PRICE lovelace from $BUYER_NAME in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at $BUYER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Buyer Reports that There is a Problem"

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe            \
                        --choice-name "Report problem"         \
                        --choice-party "PK=$BUYER_PUBKEYHASH"  \
                        --choice-number 1                      \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-3.marlowe                \
                        --print-stats

echo "Now the buyer $BUYER_NAME can submit a transaction to report that there is a problem:"

TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_2"#0              \
                        --tx-in "$TX_2"#0                         \
                        --required-signer "$BUYER_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$BUYER_ADDRESS"         \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo "The reporting of a problem was recorded in the transaction "'`'"$TX_3"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at $BUYER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. The Seller Disputes that There is a Problem"

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "PK=$SELLER_PUBKEYHASH" \
                        --choice-number 0                      \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-4.marlowe                \
                        --print-stats

echo "Now the seller $SELLER_NAME can submit a transaction to dispute that there is a problem:"

TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-3.marlowe            \
                        --tx-in-marlowe "$TX_3"#1                 \
                        --tx-in-collateral "$TX_0_SELLER"         \
                        --tx-in "$TX_0_SELLER"                    \
                        --required-signer "$SELLER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-4.marlowe           \
                        --change-address "$SELLER_ADDRESS"        \
                        --out-file tx-4.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The dispute that this is a problem is recorded in the transaction "'`'"$TX_4"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "Here is the UTxO at the seller $SELLER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "## Transaction 5. The Mediator Dismisses the Claim"

echo "Funds are released to the seller and mediator, closing the contract."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-4.marlowe              \
                        --choice-name "Dismiss claim"            \
                        --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                        --choice-number 0                        \
                        --invalid-before "$TIP"                  \
                        --invalid-hereafter "$(($TIP+4*3600))"   \
                        --out-file tx-5.marlowe                  \
                        --print-stats

echo "Now the mediator $MEDIATOR_NAME can submit a transaction to release funds:"

TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --marlowe-in-file tx-4.marlowe             \
                        --tx-in-marlowe "$TX_4"#1                  \
                        --tx-in-collateral "$TX_1"#0               \
                        --tx-in "$TX_1"#0                          \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-5.marlowe            \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --out-file tx-5.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)

echo "The dismissal of the claim resulted in closing the contract, paying $PRICE lovelace to the the seller $SELLER_NAME and $MINIMUM_ADA lovelace to the mediator $MEDIATOR_NAME in the transaction "'`'"$TX_5"'`'".  There is no UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here is the UTxO at the seller $SELLER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "There is no UTxO at the buyer $BUYER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here is the UTxO at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "## Clean Up Wallets"

echo "It's convenient to consolidate all of the UTxOs into single ones."

marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_4"#0                         \
                               --tx-in "$TX_5"#2                         \
                               --required-signer "$SELLER_PAYMENT_SKEY"  \
                               --change-address "$SELLER_ADDRESS"        \
                               --out-file tx-6.raw                       \
                               --submit=600                              \
> /dev/null
marlowe-cli transaction simple "${MAGIC[@]}"                              \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX_5"#0                         \
                               --tx-in "$TX_5"#1                         \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                               --change-address "$MEDIATOR_ADDRESS"       \
                               --out-file tx-8.raw                        \
                               --submit=600                               \
> /dev/null

