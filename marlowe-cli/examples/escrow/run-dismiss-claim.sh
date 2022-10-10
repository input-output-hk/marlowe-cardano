#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo '# Example Escrow Contract: "Dismiss Claim"'

echo "In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, and the mediator dismisses the buyer's claim."
echo
echo '![Flow chart for "dismiss claim".](dismiss-claim.svg)'

echo "## Prerequisites"

echo "The environment variable "'`'"CARDANO_NODE_SOCKET_PATH"'`'" must be set to the path to the cardano node's socket."
echo "See below for how to set "'`'"MAGIC"'`'" to select the network."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo
echo "Signing and verification keys must be provided below for the bystander and party roles, or they will be created automatically: to do this, set the environment variables "'`'"SELLER_PREFIX"'`'", "'`'"BUYER_PREFIX"'`'", and "'`'"MEDIATOR_PREFIX"'`'" where they appear below."

echo "## Preliminaries"

: "${FAUCET_ADDRESS:?FAUCET_ADDRESS not set}"
: "${FAUCET_SKEY_FILE:?FAUCET_SKEY_FILE not set}"

echo "### Select Network"

: "${MAGIC:=2}"
echo "MAGIC=$MAGIC"

SLOT_LENGTH=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(((1000 * $(date -u +%s) - SLOT_OFFSET) / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "### Participants"

echo "#### The Seller"

echo "The seller sells an item for a price."

SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_ROLE=FB
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey

echo "Create the seller's keys, if necessary."

if [[ ! -e "$SELLER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$SELLER_PAYMENT_SKEY"      \
                              --verification-key-file "$SELLER_PAYMENT_VKEY"
fi
SELLER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$SELLER_PAYMENT_VKEY")

echo "Fund the seller's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 50000000                       \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$SELLER_ADDRESS"

echo "#### The Buyer"

BUYER_PREFIX="$TREASURY/thomas-middleton"
BUYER_NAME="Thomas Middleton"
BUYER_ROLE=TM
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey

echo "Create the buyer's keys, if necessary."

if [[ ! -e "$BUYER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BUYER_PAYMENT_SKEY"      \
                              --verification-key-file "$BUYER_PAYMENT_VKEY"
fi
BUYER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$BUYER_PAYMENT_VKEY")

echo "Fund the buyer's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 350000000                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$BUYER_ADDRESS"

echo "#### The Mediator"

MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_ROLE=CM
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey

echo "Create the mediator's keys, if necessary."

if [[ ! -e "$MEDIATOR_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$MEDIATOR_PAYMENT_SKEY"      \
                              --verification-key-file "$MEDIATOR_PAYMENT_VKEY"
fi
MEDIATOR_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY")

echo "Fund the mediator's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              --submit 600                              \
                              --lovelace 120000000                      \
                              "$MEDIATOR_ADDRESS"

echo "### Role Tokens"

echo "The mediator mints the role tokens."

MINT_EXPIRES=$((TIP + 1000000))

ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"     \
                      --issuer "$MEDIATOR_ADDRESS:$MEDIATOR_PAYMENT_SKEY" \
                      --expires "$MINT_EXPIRES"                     \
                      --out-file /dev/null                          \
                      --submit=600                                  \
                      "$MEDIATOR_ROLE:$MEDIATOR_ADDRESS" \
                      "$SELLER_ROLE:$SELLER_ADDRESS" \
                      "$BUYER_ROLE:$BUYER_ADDRESS" \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                \
)

MEDIATOR_TOKEN="$ROLE_CURRENCY.$MEDIATOR_ROLE"
SELLER_TOKEN="$ROLE_CURRENCY.$SELLER_ROLE"
BUYER_TOKEN="$ROLE_CURRENCY.$BUYER_ROLE"

echo "### Available UTxOs"

echo "The mediator $MEDIATOR_NAME is the minimum-ADA provider and has the address "'`'"$MEDIATOR_ADDRESS"'`'" and role token named "'`'"$MEDIATOR_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY"\
                       --change-address "$MEDIATOR_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS"

echo "We select the UTxO with the mediator $MEDIATOR_NAME's role token."

TX_0_MEDIATOR_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 80000000                  \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_MEDIATOR_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$MEDIATOR_TOKEN"            \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$MEDIATOR_NAME will spend the UTxOs "'`'"$TX_0_MEDIATOR_ADA"'`'" and "'`'"$TX_0_MEDIATOR_TOKEN"'`.'

echo "The seller $SELLER_NAME has the address "'`'"$SELLER_ADDRESS"'`'" and role token named "'`'"$SELLER_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$SELLER_PAYMENT_SKEY"  \
                       --change-address "$SELLER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS"

echo "We select the UTxO with the lender $SELLER_NAME's role token."

TX_0_SELLER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_SELLER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$SELLER_TOKEN"              \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$SELLER_NAME will spend the UTxOs "'`'"$TX_0_SELLER_ADA"'`'" and "'`'"$TX_0_SELLER_TOKEN"'`.'

echo "The buyer $BUYER_NAME has the address "'`'"$BUYER_ADDRESS"'`'" and role token named "'`'"$BUYER_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$BUYER_PAYMENT_SKEY"   \
                       --change-address "$BUYER_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"

echo "We select the UTxO with the lender $BUYER_NAME's role token."

TX_0_BUYER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BUYER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BUYER_TOKEN"               \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$BUYER_NAME will spend the UTxOs "'`'"$TX_0_BUYER_ADA"'`'" and "'`'"$TX_0_BUYER_TOKEN"'`.'

echo "## The Contract"

echo "The contract has a minimum slot and several deadlines."

PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))

echo "* The current slot is $TIP."
echo "* The buyer $BUYER_NAME must pay before $(date -u -R -d @$((PAYMENT_DEADLINE/1000)))."
echo "* They buyer $BUYER_NAME has until $(date -u -R -d @$((COMPLAINT_DEADLINE/1000))) to complain."
echo "* The seller $SELLER_NAME has until $(date -u -R -d @$((DISPUTE_DEADLINE/1000))) to dispute a complaint."
echo "* The mediator $MEDIATOR_NAME has until $(date -u -R -d @$((MEDIATION_DEADLINE/1000))) to decide on a disputed complaint."

echo "The contract also involves the price of the good exchanged and a minimum-ADA value."

MINIMUM_ADA=3000000
PRICE=256000000

echo "The selling price is $PRICE lovelace."

echo "We create the contract for the previously specified parameters."

marlowe-cli template escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "$SELLER_ROLE"                    \
                            --buyer "$BUYER_ROLE"                      \
                            --mediator "$MEDIATOR_ROLE"                \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-contract-file tx-1.contract          \
                            --out-state-file    tx-1.state

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require."

marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --roles-currency "$ROLE_CURRENCY"         \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "Because this is a role-based contract, we compute the address of the script for roles."

ROLE_ADDRESS=$(jq -r '.tx.rolesValidator.address' tx-1.marlowe)

echo "The role address is "'`'"$ROLE_ADDRESS"'`.'

echo "The mediator $MEDIATOR_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                   \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR_ADA"               \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe            \
                        --out-file tx-1.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the mediator $MEDIATOR_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here is the UTxO at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Buyer Deposits Funds into Seller's Account."

echo "First we compute the Marlowe input required to make the initial deposit by the buyer."

marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "$SELLER_ROLE"      \
                        --deposit-party "$BUYER_ROLE"         \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats

echo "Now the buyer $BUYER_NAME submits the transaction along with their deposit:"

TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-1.marlowe                        \
                        --tx-in-marlowe "$TX_1"#1                             \
                        --tx-in-collateral "$TX_0_BUYER_ADA"                  \
                        --tx-in "$TX_0_BUYER_ADA"                             \
                        --tx-in "$TX_0_BUYER_TOKEN"                           \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-2.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-2.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                              \
)

echo "The contract received the deposit of $PRICE lovelace from $BUYER_NAME in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at $BUYER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Buyer Reports that There is a Problem"

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Report problem"        \
                        --choice-party "$BUYER_ROLE"          \
                        --choice-number 1                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats

echo "Now the buyer $BUYER_NAME can submit a transaction to report that there is a problem:"

TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-2.marlowe                        \
                        --tx-in-marlowe "$TX_2"#1                             \
                        --tx-in-collateral "$TX_2"#0                          \
                        --tx-in "$TX_2"#0                                     \
                        --tx-in "$TX_2"#2                                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-3.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-3.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)

echo "The reporting of a problem was recorded in the transaction "'`'"$TX_3"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at $BUYER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. The Seller Disputes that There is a Problem"

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "$SELLER_ROLE"      \
                        --choice-number 0                      \
                        --invalid-before "$NOW"                \
                        --invalid-hereafter "$((NOW+4*HOUR))"  \
                        --out-file tx-4.marlowe                \
                        --print-stats

echo "Now the seller $SELLER_NAME can submit a transaction to dispute that there is a problem:"

TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-3.marlowe                          \
                        --tx-in-marlowe "$TX_3"#1                               \
                        --tx-in-collateral "$TX_0_SELLER_ADA"                   \
                        --tx-in "$TX_0_SELLER_ADA"                              \
                        --tx-in "$TX_0_SELLER_TOKEN"                            \
                        --required-signer "$SELLER_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-4.marlowe                         \
                        --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                        --change-address "$SELLER_ADDRESS"                      \
                        --out-file tx-4.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)

echo "The dispute that this is a problem is recorded in the transaction "'`'"$TX_4"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "Here is the UTxO at the seller $SELLER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "## Transaction 5. The Mediator Dismisses the Claim"

echo "Funds are released to the seller and mediator, closing the contract."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Dismiss claim"         \
                        --choice-party "$MEDIATOR_ROLE"       \
                        --choice-number 0                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-5.marlowe               \
                        --print-stats

echo "Now the mediator $MEDIATOR_NAME can submit a transaction to release funds:"

TX_5=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-4.marlowe                              \
                        --tx-in-marlowe "$TX_4"#1                                   \
                        --tx-in-collateral "$TX_1"#0                                \
                        --tx-in "$TX_1"#0                                           \
                        --tx-in "$TX_0_MEDIATOR_TOKEN"                              \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-5.marlowe                             \
                        --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                        --change-address "$MEDIATOR_ADDRESS"                        \
                        --out-file tx-5.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)

echo "The dismissal of the claim resulted in closing the contract, paying $PRICE lovelace to the role address for the benefit of the seller $SELLER_NAME and $MINIMUM_ADA lovelace for the benefit of the mediator $MEDIATOR_NAME in the transaction "'`'"$TX_5"'`'".  There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"

echo "Here are the UTxOs at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here is the UTxO at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "## Transactions 6 and 7. Seller and Mediator Withdraw Funds."

echo "The seller $SELLER_NAME submits a transaction to withdraw the payment from the role address."

TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-5.marlowe                             \
                         --role-name "$SELLER_ROLE"                              \
                         --tx-in "$TX_4"#0                                       \
                         --tx-in "$TX_4"#2                                       \
                         --tx-in-collateral "$TX_4"#0                            \
                         --required-signer "$SELLER_PAYMENT_SKEY"                \
                         --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                         --change-address "$SELLER_ADDRESS"                      \
                         --out-file tx-6.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)

echo "The mediator $MEDIATOR_NAME submits a transaction to withdraw the minimum ADA from the role address."

TX_7=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                    \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-5.marlowe                                 \
                         --role-name "$MEDIATOR_ROLE"                                \
                         --tx-in "$TX_5"#0                                           \
                         --tx-in "$TX_5"#3                                           \
                         --tx-in-collateral "$TX_5"#0                                \
                         --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                         --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                         --change-address "$MEDIATOR_ADDRESS"                        \
                         --out-file tx-7.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)

echo "There is no UTxO at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "Here are the UTxOs at the seller $SELLER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "Here are the UTxOs at the buyer $BUYER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "Here are the UTxOs at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "Burning tokens issued by MEDIATOR:"

marlowe-cli util burn --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$MEDIATOR_ADDRESS:$MEDIATOR_PAYMENT_SKEY" \
                      --expires $MINT_EXPIRES \
                      --token-provider "$MEDIATOR_ADDRESS:$MEDIATOR_PAYMENT_SKEY" \
                      --token-provider "$SELLER_ADDRESS:$SELLER_PAYMENT_SKEY" \
                      --token-provider "$BUYER_ADDRESS:$BUYER_PAYMENT_SKEY" \
                      --submit 600 \
                      --out-file /dev/null


echo "Sending back funds:"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$MEDIATOR_ADDRESS:$MEDIATOR_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$SELLER_ADDRESS:$SELLER_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$BUYER_ADDRESS:$BUYER_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

echo "## AFTER CLEANUP"

echo "Here are the UTxOs at the seller $SELLER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS"

echo "Here are the UTxOs at the buyer $BUYER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"

echo "Here are the UTxOs at the mediator $MEDIATOR_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS"

