#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo "# Test of a Covered Call Contract"
echo "[This option contract](../../../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option."

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
echo "Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables "'`'"ISSUER_PREFIX"'`'" and "'`'"COUNTERPARTY_PREFIX"'`'" where they appear below."
echo
echo "The two parties' wallets much have exactly one UTxO with the token they want to swap and at least one UTxO without tokens."

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

echo "#### The Issuer"

ISSUER_PREFIX="$TREASURY/john-fletcher"
ISSUER_NAME="John Fletcher"
ISSUER_PAYMENT_SKEY="$ISSUER_PREFIX".skey
ISSUER_PAYMENT_VKEY="$ISSUER_PREFIX".vkey

echo "Create the issuer's keys, if necessary."

if [[ ! -e "$ISSUER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$ISSUER_PAYMENT_SKEY"      \
                              --verification-key-file "$ISSUER_PAYMENT_VKEY"
fi
ISSUER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$ISSUER_PAYMENT_VKEY" )

echo "Fund the issuer's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --out-file /dev/null                      \
                                --submit 600                              \
                                --lovelace 150000000                      \
                                --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                                "$ISSUER_ADDRESS"

echo "The issuer mints their tokens for the contract."

MINT_EXPIRES=$((TIP + 1000000))

TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                          \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --issuer "$ISSUER_ADDRESS:$ISSUER_PAYMENT_SKEY"   \
                      --count "$AMOUNT_A"                               \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$TOKEN_NAME_A:$ISSUER_ADDRESS"                   \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                    \
)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"

echo "The issuer $ISSUER_NAME is the minimum-ADA provider and has the address "'`'"$ISSUER_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ISSUER_PAYMENT_SKEY"  \
                       --change-address "$ISSUER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_A_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$ISSUER_NAME will spend the UTxOs "'`'"$TX_0_A_ADA"'`'" and "'`'"$TX_0_A_TOKEN"'`'". They will trade $AMOUNT_A of "'`'"$TOKEN_A"'`.'

echo "#### The Counterparty"

COUNTERPARTY_PREFIX="$TREASURY/thomas-kyd"
COUNTERPARTY_NAME="Thomas Kyd"
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey

echo "Create the counterparty's keys, if necessary."

if [[ ! -e "$COUNTERPARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
fi
COUNTERPARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" )

echo "Fund the counterparty's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                        "$COUNTERPARTY_ADDRESS"

echo "The counterparty mints their tokens for the swap."

TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                       \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                      --issuer "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                      --count "$AMOUNT_B"                            \
                      --expires "$MINT_EXPIRES"                      \
                      --out-file /dev/null                           \
                      --submit=600                                   \
                      "$TOKEN_NAME_B:$COUNTERPARTY_ADDRESS"          \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                 \
)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"

echo "The counterparty $COUNTERPARTY_NAME has the address "'`'"$COUNTERPARTY_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                       \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_B_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$COUNTERPARTY_NAME will spend the UTxOs "'`'"$TX_0_B_ADA"'`'" and "'`'"$TX_0_B_TOKEN"'`'". They will trade $AMOUNT_B of "'`'"$TOKEN_B"'`.'

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(($(date -u +%s) - SLOT_OFFSET / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

MINIMUM_ADA=3000000

ISSUE_TIMEOUT=$((NOW+5*HOUR))
MATURITY_TIMEOUT=$((NOW+0*HOUR))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))

echo "The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer $ISSUER_NAME will put $AMOUNT_A of "'`'"$TOKEN_A"'`'" before $(date -u -R -d @$((ISSUE_TIMEOUT/1000))) into the contract and if the counter-party $COUNTERPARTY_NAME exercises the option for $AMOUNT_B of "'`'"$TOKEN_B"'`'" after $(date -u -R -d @$((MATURITY_TIMEOUT/1000))) and before $(date -u -R -d @$((SETTLEMENT_TIMEOUT/1000)))."

echo "We create the contract for the previously specified parameters."

marlowe-cli template coveredCall --minimum-ada "$MINIMUM_ADA"                  \
                                 --issuer "$ISSUER_ADDRESS"                    \
                                 --counter-party "$COUNTERPARTY_ADDRESS"       \
                                 --currency "$TOKEN_B"                         \
                                 --underlying "$TOKEN_A"                       \
                                 --strike "$AMOUNT_B"                          \
                                 --amount "$AMOUNT_A"                          \
                                 --issue-date "$ISSUE_TIMEOUT"                 \
                                 --maturity-date "$MATURITY_TIMEOUT"           \
                                 --settlement-date "$SETTLEMENT_TIMEOUT"       \
                                 --out-contract-file tx-1.contract             \
                                 --out-state-file    tx-1.state


echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require."

marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "The issuer $ISSUER_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_A_ADA"                     \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the issuer $ISSUER_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the issuer $ISSUER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. The issuer deposits Tokens into the Contract."

echo "First we compute the Marlowe input required to deposit the tokens."

marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "$ISSUER_ADDRESS"       \
                        --deposit-party "$ISSUER_ADDRESS"         \
                        --deposit-token "$TOKEN_A"                \
                        --deposit-amount "$AMOUNT_A"              \
                        --invalid-before "$NOW"                   \
                        --invalid-hereafter "$((NOW+1*HOUR))"     \
                        --out-file tx-2.marlowe                   \
                        --print-stats

echo "Now the issuer $ISSUER_NAME submits the transaction that deposits their tokens."

TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_1"#0              \
                        --tx-in "$TX_1"#0                         \
                        --tx-in "$TX_0_A_TOKEN"                   \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the deposit of $AMOUNT_A "'`'"$TOKEN_A"'`'" in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the issuer $ISSUER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Counter-Party chooses to exercise the option"

marlowe-cli run prepare --marlowe-file tx-2.marlowe            \
                        --choice-name "Exercise Call"          \
                        --choice-party "$COUNTERPARTY_ADDRESS" \
                        --choice-number 1                      \
                        --invalid-before "$NOW"                \
                        --invalid-hereafter "$((NOW+1*HOUR))"  \
                        --out-file tx-3.marlowe                \
                        --print-stats


TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                       \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-2.marlowe                 \
                        --tx-in-marlowe "$TX_2"#1                      \
                        --tx-in-collateral "$TX_0_B_ADA"               \
                        --tx-in "$TX_0_B_ADA"                          \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-3.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-3.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)

echo "## Transaction 4. The Counter-Party Deposits their Tokens."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-3.marlowe                     \
                        --deposit-account "$COUNTERPARTY_ADDRESS"       \
                        --deposit-party "$COUNTERPARTY_ADDRESS"         \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$NOW"                         \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-4.marlowe                         \
                        --print-stats

echo "Now the counter-party $COUNTERPARTY_NAME can submit a transaction that deposits their tokens."

TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                       \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-3.marlowe                 \
                        --tx-in-marlowe "$TX_3"#1                      \
                        --tx-in-collateral "$TX_3"#0                   \
                        --tx-in "$TX_3"#0                              \
                        --tx-in "$TX_0_B_TOKEN"                        \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-4.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-4.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)

echo "The closing of the contract paid $AMOUNT_B "'`'"$TOKEN_B"'`'" to the first party $ISSUER_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract, and it paid $AMOUNT_A "'`'"$TOKEN_A"'`'" to the counter party $COUNTERPARTY_NAME in the transaction "'`'"$TX_4"'`'". There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "Here are the UTxOs at the issuer party $ISSUER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "Here are the UTxOs at the counter party $COUNTERPARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "## Clean Up"

echo "Burning tokens issued by ISSUER:"

marlowe-cli -- util burn --testnet-magic "$MAGIC" \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                         --issuer "$ISSUER_ADDRESS:$ISSUER_PAYMENT_SKEY"   \
                         --expires $MINT_EXPIRES \
                         --token-provider "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                          --token-provider "$ISSUER_ADDRESS:$ISSUER_PAYMENT_SKEY" \
                         --submit 600 \
                         --out-file /dev/null

echo "Burning tokens issued by COUNTERPARTY:"

marlowe-cli util burn --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                      --expires $MINT_EXPIRES \
                      --token-provider "$ISSUER_ADDRESS:$ISSUER_PAYMENT_SKEY" \
                      --token-provider "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                      --submit 600 \
                      --out-file /dev/null


echo "Sending back funds:"

marlowe-cli util fund-address --testnet-magic "$MAGIC" \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --send-all \
                              --source-wallet-credentials "$ISSUER_ADDRESS:$ISSUER_PAYMENT_SKEY" \
                              --submit 600 \
                              --out-file /dev/null \
                              "$FAUCET_ADDRESS"

marlowe-cli util fund-address --testnet-magic "$MAGIC" \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --send-all \
                              --source-wallet-credentials "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                              --submit 600 \
                              --out-file /dev/null \
                              "$FAUCET_ADDRESS"

echo "Here are the UTxOs at the second party $ISSUER_NAME's address after the cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS"

echo "Here are the UTxOs at the second party $COUNTERPARTY_NAME's address after the cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"
