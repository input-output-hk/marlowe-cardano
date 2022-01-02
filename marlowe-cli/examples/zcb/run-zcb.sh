#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -e

echo "# Test of a Zero-Coupon Bond"

echo "[This zero-coupon bond](../../src/Language/Marlowe/CLI/Examples/ZeroCouponBond.hs) has one party borrow and another pay back with interest. It uses role tokens."

echo "## Prerequisites"

echo "The environment variable "'`'"CARDANO_NODE_SOCKET_PATH"'`'" must be set to the path to the cardano node's socket."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo '* xargs'
echo
echo "Signing and verification keys must be provided below for the two parties: to do this, set the environment variables "'`'"LENDER_PREFIX"'`'" and "'`'"BORROWER_PREFIX"'`'" where they appear below."
echo
echo "The two parties' wallets must have exactly one UTxO with their role token. The currency symbol for the role tokens must be set below in "'`'"ROLE_CURRENCY"'`'"."

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

echo "### Role Currency"

echo "Set the role currency for the validator."

ROLE_CURRENCY=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d

echo "#### The Lender"

LENDER_PREFIX="$TREASURY/john-fletcher"
LENDER_NAME="John Fletcher"
LENDER_ROLE=JohnFletcher
LENDER_TOKEN="$ROLE_CURRENCY.$LENDER_ROLE"
LENDER_PAYMENT_SKEY="$LENDER_PREFIX".skey
LENDER_PAYMENT_VKEY="$LENDER_PREFIX".vkey
LENDER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$LENDER_PAYMENT_VKEY" \
)
LENDER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$LENDER_PAYMENT_VKEY"
)

echo "The lender $LENDER_NAME is the minimum-ADA provider and has the address "'`'"$LENDER_ADDRESS"'`'" and public-key hash "'`'"$LENDER_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$LENDER_PAYMENT_SKEY"  \
                       --change-address "$LENDER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS"

echo "We select the UTxO with the lender $LENDER_NAME's role token."

TX_0_LENDER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$LENDER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_LENDER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$LENDER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$LENDER_ROLE"'" == 1) | .key' \
)

echo "$LENDER_NAME will spend the UTxOs "'`'"$TX_0_LENDER_ADA"'`'" and "'`'"$TX_0_LENDER_TOKEN"'`.'

echo "### The Borrower"

BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=ThomasMiddleton
BORROWER_TOKEN="$ROLE_CURRENCY.$BORROWER_ROLE"
BORROWER_PAYMENT_SKEY="$BORROWER_PREFIX".skey
BORROWER_PAYMENT_VKEY="$BORROWER_PREFIX".vkey
BORROWER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$BORROWER_PAYMENT_VKEY" \
)
BORROWER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BORROWER_PAYMENT_VKEY"
)

echo "The borrower $BORROWER_NAME has the address "'`'"$BORROWER_ADDRESS"'`'" and public-key hash "'`'"$BORROWER_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                              \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$BORROWER_PAYMENT_SKEY" \
                       --change-address "$BORROWER_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS"

echo "We select the UTxO with the borrower $BORROWER_NAME's role token."

TX_0_BORROWER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BORROWER_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_BORROWER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                        \
                       --address "$BORROWER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                               \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BORROWER_ROLE"'" == 1) | .key' \
)

echo "$BORROWER_NAME will spend the UTxOs "'`'"$TX_0_BORROWER_ADA"'`'" and "'`'"$TX_0_BORROWER_TOKEN"'`.'

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(($(date -u +%s) - SLOT_OFFSET / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000

LENDING_DEADLINE=$((TIP+12*3600))
REPAYMENT_DEADLINE=$((TIP+24*3600))

echo "The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender $LENDER_NAME will pay principal of $PRINCIPAL ADA before slot $LENDING_DEADLINE and the borrower will repay the principal and interest of $INTEREST ADA before slot $REPAYMENT_DEADLINE."

echo "We create the contract for the previously specified parameters."

marlowe-cli template zcb --minimum-ada "$MINIMUM_ADA"               \
                         --lender "Role=$LENDER_ROLE"               \
                         --borrower "Role=$BORROWER_ROLE"           \
                         --principal "$PRINCIPAL"                   \
                         --interest "$INTEREST"                     \
                         --lending-deadline "$LENDING_DEADLINE"     \
                         --repayment-deadline "$REPAYMENT_DEADLINE" \
                         --out-contract-file tx-1.contract          \
                         --out-state-file    tx-1.state

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require."

marlowe-cli run initialize "${MAGIC[@]}"                     \
                           --slot-length "$SLOT_LENGTH"      \
                           --slot-offset "$SLOT_OFFSET"      \
                           --roles-currency "$ROLE_CURRENCY" \
                           --contract-file tx-1.contract     \
                           --state-file    tx-1.state        \
                           --out-file      tx-1.marlowe      \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "Because this is a role-based contract, we compute the address of the script for roles."

ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)

echo "The role address is "'`'"$ROLE_ADDRESS"'`.'

echo "The lender $LENDER_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace requiredd for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_LENDER_ADA"                \
                        --required-signer "$LENDER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$LENDER_ADDRESS"        \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the lender $LENDER_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the lender $LENDER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Lender Deposits the Loan Amount"

echo "First we compute the Marlowe input required to deposit the funds for the loan."

marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$LENDER_ROLE" \
                        --deposit-party "Role=$LENDER_ROLE"   \
                        --deposit-amount "$PRINCIPAL"         \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-2.marlowe               \
                        --print-stats

echo "Now the lender $LENDER_NAME submits the transaction that deposits the loan amount."

TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                                           \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-1.marlowe                          \
                        --tx-in-marlowe "$TX_1"#1                               \
                        --tx-in-collateral "$TX_1"#0                            \
                        --tx-in "$TX_1"#0                                       \
                        --tx-in "$TX_0_LENDER_TOKEN"                            \
                        --required-signer "$LENDER_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-2.marlowe                         \
                        --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                        --change-address "$LENDER_ADDRESS"                      \
                        --out-file tx-2.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)

echo "The contract passed the deposit of $PRINCIPAL ADA in the transaction "'`'"$TX_2"'`'" from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the lender $LENDER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the role address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. Lender Withdraws Loan."

echo "The lender $LENDER_NAME submits a transaction to withdraw the loan from the role address."

TX_3=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                               \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-2.marlowe                                 \
                         --role-name "$BORROWER_ROLE"                                \
                         --tx-in "$TX_0_BORROWER_TOKEN"                              \
                         --tx-in "$TX_0_BORROWER_ADA"                                \
                         --tx-in-collateral "$TX_0_BORROWER_ADA"                     \
                         --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                         --change-address "$BORROWER_ADDRESS"                        \
                         --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                         --out-file tx-3.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)

echo "There is no UTxO at the role address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at the borrower $BORROWER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. Borrower Repays the Loan's Principal and Interest"

echo "First we compute the Marlowe input required to replay the funds for the loan."

marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "Role=$BORROWER_ROLE"    \
                        --deposit-party "Role=$BORROWER_ROLE"      \
                        --deposit-amount "$((PRINCIPAL+INTEREST))" \
                        --invalid-before "$TIP"                    \
                        --invalid-hereafter "$((TIP+4*3600))"      \
                        --out-file tx-4.marlowe                    \
                        --print-stats

echo "Now the borrower $BORROWER_NAME submits a transaction that repays the loan."

TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                               \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-2.marlowe                              \
                        --tx-in-marlowe "$TX_2"#1                                   \
                        --tx-in "$TX_3"#0                                           \
                        --tx-in "$TX_3"#1                                           \
                        --tx-in "$TX_3"#2                                           \
                        --tx-in-collateral "$TX_3"#0                                \
                        --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-4.marlowe                             \
                        --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                        --change-address "$BORROWER_ADDRESS"                        \
                        --out-file tx-4.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)

echo "The closing of the contract paid in the transaction "'`'"$TX_4"'`'" the $PRINCIPAL ADA principal and $INTEREST ADA interest to the role address for the benefit of the lender $LENDER_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract. There is no UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "Here are the UTxOs at the borrower $BORROWER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "Here is the UTxO at the role address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "## Transaction 5. Lender Withdraws Repayment."

echo "The lender $LENDER_NAME submits a transaction to withdraw the repayment from the role address."

TX_5=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-4.marlowe                             \
                         --role-name "$LENDER_ROLE"                              \
                         --tx-in "$TX_2"#0                                       \
                         --tx-in "$TX_2"#3                                       \
                         --tx-in-collateral "$TX_2"#0                            \
                         --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                         --change-address "$LENDER_ADDRESS"                      \
                         --required-signer "$LENDER_PAYMENT_SKEY"                \
                         --out-file tx-5.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)

echo "There is no UTxO at the role address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here are the UTxOs at the lender $LENDER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

