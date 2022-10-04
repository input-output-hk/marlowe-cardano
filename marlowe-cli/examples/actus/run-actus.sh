#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo "# Test of an ACTUS Contract (Zero-Coupon Bond)"

echo "This zero-coupon bond has one party borrow and another pay back with interest. It uses role tokens."

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
echo "Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables "'`'"LENDER_PREFIX"'`'" and "'`'"BORROWER_PREFIX"'`'" where they appear below."

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

echo "#### The Lender"

LENDER_PREFIX="$TREASURY/john-fletcher"
LENDER_NAME="John Fletcher"
LENDER_ROLE=PA
LENDER_PAYMENT_SKEY="$LENDER_PREFIX".skey
LENDER_PAYMENT_VKEY="$LENDER_PREFIX".vkey

echo "Create the lender's keys, if necessary."

if [[ ! -e "$LENDER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$LENDER_PAYMENT_SKEY"      \
                              --verification-key-file "$LENDER_PAYMENT_VKEY"
fi
LENDER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$LENDER_PAYMENT_VKEY")

echo "Fund the lender's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 150000000                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$LENDER_ADDRESS"

echo "#### The Borrower"

BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=CP
BORROWER_PAYMENT_SKEY="$BORROWER_PREFIX".skey
BORROWER_PAYMENT_VKEY="$BORROWER_PREFIX".vkey

echo "Create the borrower's keys, if necessary."

if [[ ! -e "$BORROWER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BORROWER_PAYMENT_SKEY"      \
                              --verification-key-file "$BORROWER_PAYMENT_VKEY"
fi
BORROWER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$BORROWER_PAYMENT_VKEY")

echo "Fund the borrower's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 250000000                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$BORROWER_ADDRESS"

echo "### Role Tokens"

echo "The lender mints the role tokens."

MINT_EXPIRES=$((TIP + 1000000))

ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$LENDER_ADDRESS:$LENDER_PAYMENT_SKEY" \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$LENDER_ROLE:$LENDER_ADDRESS"            \
                      "$BORROWER_ROLE:$BORROWER_ADDRESS"        \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)

LENDER_TOKEN="$ROLE_CURRENCY.$LENDER_ROLE"
BORROWER_TOKEN="$ROLE_CURRENCY.$BORROWER_ROLE"

echo "Find the transaction output with the borrower's role token."

echo "### Available UTxOs"

echo "The lender $LENDER_NAME is the minimum-ADA provider and has the address "'`'"$LENDER_ADDRESS"'`'" and role token named "'`'"$LENDER_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$LENDER_PAYMENT_SKEY"  \
                       --change-address "$LENDER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS"

echo "We select the UTxO with the lender $LENDER_NAME's role token."

TX_0_LENDER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 120000000                 \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_LENDER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$LENDER_TOKEN"              \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$LENDER_NAME will spend the UTxOs "'`'"$TX_0_LENDER_ADA"'`'" and "'`'"$TX_0_LENDER_TOKEN"'`.'

echo "The borrower $BORROWER_NAME has the address "'`'"$BORROWER_ADDRESS"'`'" and role token named "'`'"$BORROWER_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                   \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$BORROWER_PAYMENT_SKEY" \
                       --change-address "$BORROWER_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS"

echo "We select the UTxO with the lender $BORROWER_NAME's role token."

TX_0_BORROWER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BORROWER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BORROWER_TOKEN"            \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$BORROWER_NAME will spend the UTxOs "'`'"$TX_0_BORROWER_ADA"'`'" and "'`'"$TX_0_BORROWER_TOKEN"'`.'

echo "## The Contract"

MINIMUM_ADA=3000000

FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02
INTEREST=$(jq -n $PRINCIPAL*$INTEREST_RATE)

STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")

LENDING_DEADLINE=$((NOW+12*HOUR))
REPAYMENT_DEADLINE=$((NOW+24*HOUR))

ACTUS_TERMS_FILE=$(mktemp)
cat > "$ACTUS_TERMS_FILE" << EOF
{
  "scheduleConfig": {
    "businessDayConvention": "NULL",
    "endOfMonthConvention": "EOM",
    "calendar": "NC"
  },
  "maturityDate": "$MATURITY_DATE",
  "contractId": "0",
  "enableSettlement": false,
  "initialExchangeDate": "$INITIAL_EXCHANGE_DATE",
  "contractRole": "RPA",
  "penaltyType": "O",
  "cycleAnchorDateOfInterestPayment": "$INITIAL_EXCHANGE_DATE",
  "contractType": "PAM",
  "notionalPrincipal": $PRINCIPAL,
  "contractPerformance": "PF",
  "collateralAmount": 0,
  "dayCountConvention": "30E360",
  "accruedInterest": 0,
  "statusDate": "$STATUS_DATE",
  "cycleOfInterestPayment": "P1YL1",
  "prepaymentEffect": "N",
  "nominalInterestRate": $INTEREST_RATE,
  "interestCalculationBase": "NT"
}
EOF

echo "The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender $LENDER_NAME will pay principal of $PRINCIPAL ada before $(date -u -R -d @$((LENDING_DEADLINE/1000))) and the borrower will repay the principal and interest of $INTEREST ada before $(date -u -R -d @$((REPAYMENT_DEADLINE/1000)))."

echo "We create the contract for the previously specified parameters."

marlowe-cli template actus --minimum-ada "$MINIMUM_ADA"               \
                           --party "$LENDER_ROLE"                     \
                           --counter-party "$BORROWER_ROLE"           \
                           --actus-terms-file "$ACTUS_TERMS_FILE"     \
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

echo "The lender $LENDER_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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

echo "The contract received the minimum ADA amount of $MINIMUM_ADA lovelace from the lender $LENDER_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the lender $LENDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Lender Deposits the Loan Amount"

echo "First we compute the Marlowe input required to deposit the funds for the loan."

marlowe-cli run prepare --marlowe-file tx-1.marlowe                   \
                        --deposit-account "$LENDER_ROLE"              \
                        --deposit-party "$LENDER_ROLE"                \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-2.marlowe                       \
                        --print-stats

echo "Now the lender $LENDER_NAME submits the transaction that deposits the loan amount."

TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                \
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

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the lender $LENDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. Lender Withdraws Loan."

echo "The lender $LENDER_NAME submits a transaction to withdraw the loan from the role address."

TX_3=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                    \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-2.marlowe                                 \
                         --role-name "$BORROWER_ROLE"                                \
                         --tx-in "$TX_0_BORROWER_TOKEN"                              \
                         --tx-in "$TX_0_BORROWER_ADA"                                \
                         --tx-in-collateral "$TX_0_BORROWER_ADA"                     \
                         --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                         --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                         --change-address "$BORROWER_ADDRESS"                        \
                         --out-file tx-3.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)

echo "There is no UTxO at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at the borrower $BORROWER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. Borrower Repays the Loan's Interest"

echo "First we compute the Marlowe input required to repay the intest of the loan."

marlowe-cli run prepare --marlowe-file tx-2.marlowe                  \
                        --deposit-account "$BORROWER_ROLE"           \
                        --deposit-party "$BORROWER_ROLE"             \
                        --deposit-amount "$((INTEREST*FIXED_POINT))" \
                        --invalid-before "$NOW"                      \
                        --invalid-hereafter "$((NOW+4*HOUR))"        \
                        --out-file tx-4.marlowe                      \
                        --print-stats

echo "Now the borrower $BORROWER_NAME submits a transaction that repays the interest of the loan."

TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
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

echo "## Transaction 5. Borrower Repays the Loan's Principal"

echo "First we compute the Marlowe input required to repay the principal of the loan."

marlowe-cli run prepare --marlowe-file tx-4.marlowe                   \
                        --deposit-account "$BORROWER_ROLE"            \
                        --deposit-party "$BORROWER_ROLE"              \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-5.marlowe                       \
                        --print-stats

echo "Now the borrower $BORROWER_NAME submits a transaction that repays the loan."

TX_5=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-4.marlowe                              \
                        --tx-in-marlowe "$TX_4"#1                                   \
                        --tx-in "$TX_4"#0                                           \
                        --tx-in "$TX_4"#3                                           \
                        --tx-in-collateral "$TX_4"#0                                \
                        --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-5.marlowe                             \
                        --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                        --change-address "$BORROWER_ADDRESS"                        \
                        --out-file tx-5.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)

echo "The closing of the contract paid in the transaction "'`'"$TX_5"'`'" the $PRINCIPAL ada principal and $INTEREST ada interest to the role address for the benefit of the lender $LENDER_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract. There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"

echo "Here are the UTxOs at the borrower $BORROWER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here is the UTxO at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "## Transaction 6. Lender Withdraws Repayment."

echo "The lender $LENDER_NAME submits a transaction to withdraw the repayment from the role address."

TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-5.marlowe                             \
                         --role-name "$LENDER_ROLE"                              \
                         --tx-in "$TX_2"#0                                       \
                         --tx-in "$TX_2"#3                                       \
                         --tx-in-collateral "$TX_2"#0                            \
                         --required-signer "$LENDER_PAYMENT_SKEY"                \
                         --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                         --change-address "$LENDER_ADDRESS"                      \
                         --out-file tx-6.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)

echo "There is no UTxO at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "Here are the UTxOs at the lender $LENDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "Here are the UTxOs at the borrower $BORROWER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "## Clean Up"

echo "Burning tokens issued by FAUCET:"

marlowe-cli util burn --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$LENDER_ADDRESS:$LENDER_PAYMENT_SKEY" \
                      --expires $MINT_EXPIRES \
                      --token-provider "$LENDER_ADDRESS:$LENDER_PAYMENT_SKEY" \
                      --token-provider "$BORROWER_ADDRESS:$BORROWER_PAYMENT_SKEY" \
                      --submit 600 \
                      --out-file /dev/null


echo "Sending back funds:"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$LENDER_ADDRESS:$LENDER_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$BORROWER_ADDRESS:$BORROWER_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

echo "Here are the UTxOs at the lender $LENDER_NAME's address after cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "Here are the UTxOs at the borrower $BORROWER_NAME's address after cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

