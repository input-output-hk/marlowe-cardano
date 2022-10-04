#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo "# Test of a Swap Contract"

echo "[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties."

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
echo "Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables "'`'"PARTY_A_PREFIX"'`'" and "'`'"PARTY_B_PREFIX"'`'" where they appear below."

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

echo "#### The First Party"

PARTY_A_PREFIX="$TREASURY/john-fletcher"
PARTY_A_NAME="John Fletcher"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey

echo "Create the first party's keys, if necessary."

if [[ ! -e "$PARTY_A_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_A_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_A_PAYMENT_VKEY"
fi
PARTY_A_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" )

echo "Fund the first party's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                 \
                        --out-file /dev/null                                      \
                        --submit 600                                              \
                        --lovelace 100000000                                      \
                        --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                        "$PARTY_A_ADDRESS"

echo "The first party mints their tokens for the swap."

MINT_EXPIRES=$((TIP + 1000000))

TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                          \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --issuer "$PARTY_A_ADDRESS:$PARTY_A_PAYMENT_SKEY" \
                      --count "$AMOUNT_A"                               \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$TOKEN_NAME_A:$PARTY_A_ADDRESS"                  \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                    \
)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"

echo "The first party $PARTY_A_NAME is the minimum-ADA provider and has the address "'`'"$PARTY_A_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_A_PAYMENT_SKEY" \
                       --change-address "$PARTY_A_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_A_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$PARTY_A_NAME will spend the UTxOs "'`'"$TX_0_A_ADA"'`'" and "'`'"$TX_0_A_TOKEN"'`'". They will trade $AMOUNT_A of "'`'"$TOKEN_A"'`.'

echo "#### The Second Party"

PARTY_B_PREFIX="$TREASURY/thomas-kyd"
PARTY_B_NAME="Thomas Kyd"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey

echo "Create the second party's keys, if necessary."

if [[ ! -e "$PARTY_B_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_B_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_B_PAYMENT_VKEY"
fi
PARTY_B_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" )

echo "Fund the second party's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 100000000                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$PARTY_B_ADDRESS"

echo "The second party mints their tokens for the swap."

TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$PARTY_B_ADDRESS:$PARTY_B_PAYMENT_SKEY" \
                      --count "$AMOUNT_B"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_B:$PARTY_B_ADDRESS"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"

echo "The second party $PARTY_B_NAME has the address "'`'"$PARTY_B_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_B_PAYMENT_SKEY" \
                       --change-address "$PARTY_B_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_B_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

echo "$PARTY_B_NAME will spend the UTxOs "'`'"$TX_0_B_ADA"'`'" and "'`'"$TX_0_B_TOKEN"'`'". They will trade $AMOUNT_B of "'`'"$TOKEN_B"'`.'

echo "## The Contract"

MINIMUM_ADA=3000000

PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))

echo "The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party $PARTY_A_NAME will swap $AMOUNT_A of "'`'"$TOKEN_A"'`'" before $(date -u -R -d @$((PARTY_A_TIMEOUT/1000))) for $AMOUNT_B of "'`'"$TOKEN_B"'`'" from the second party $PARTY_B_NAME before $(date -u -R -d @$((PARTY_B_TIMEOUT/1000)))."

echo "We create the contract for the previously specified parameters."

marlowe-cli template swap --minimum-ada "$MINIMUM_ADA"       \
                          --a-party "$PARTY_A_ADDRESS"       \
                          --a-token "$TOKEN_A"               \
                          --a-amount "$AMOUNT_A"             \
                          --a-timeout "$PARTY_A_TIMEOUT"     \
                          --b-party "$PARTY_B_ADDRESS"       \
                          --b-token "$TOKEN_B"               \
                          --b-amount "$AMOUNT_B"             \
                          --b-timeout "$PARTY_B_TIMEOUT"     \
                          --out-contract-file tx-1.contract  \
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

echo "The first party $PARTY_A_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_A_ADA"                     \
                        --required-signer "$PARTY_A_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$PARTY_A_ADDRESS"       \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the first party $PARTY_A_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. First Party Deposits Tokens into the Contract."

echo "First we compute the Marlowe input required to deposit the tokens."

marlowe-cli run prepare --marlowe-file tx-1.marlowe                \
                        --deposit-account "$PARTY_A_ADDRESS"       \
                        --deposit-party "$PARTY_A_ADDRESS"         \
                        --deposit-token "$TOKEN_A"                 \
                        --deposit-amount "$AMOUNT_A"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-2.marlowe                    \
                        --print-stats

echo "Now the first party $PARTY_A_NAME submits the transaction that deposits their tokens."

TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_1"#0              \
                        --tx-in "$TX_1"#0                         \
                        --tx-in "$TX_0_A_TOKEN"                   \
                        --required-signer "$PARTY_A_PAYMENT_SKEY" \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$PARTY_A_ADDRESS"       \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the deposit of $AMOUNT_A "'`'"$TOKEN_A"'`'" in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "$PARTY_B_ADDRESS"       \
                        --deposit-party "$PARTY_B_ADDRESS"         \
                        --deposit-token "$TOKEN_B"                 \
                        --deposit-amount "$AMOUNT_B"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-3.marlowe                    \
                        --print-stats

echo "Now the second party $PARTY_B_NAME can submit a transaction that deposits their tokens and completes the swap."

TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_0_B_ADA"          \
                        --tx-in "$TX_0_B_ADA"                     \
                        --tx-in "$TX_0_B_TOKEN"                   \
                        --required-signer "$PARTY_B_PAYMENT_SKEY" \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$PARTY_B_ADDRESS"       \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The closing of the contract paid $AMOUNT_B "'`'"$TOKEN_B"'`'" to the first party $PARTY_A_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract, and it paid $AMOUNT_A "'`'"$TOKEN_A"'`'" to the second party $PARTY_B_NAME in the transaction "'`'"$TX_3"'`'". There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here are the UTxOs at the second party $PARTY_B_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "## Clean Up"

echo "Burning tokens issued by PARTY_A:"

marlowe-cli -- util burn --testnet-magic "$MAGIC" \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                         --issuer "$PARTY_A_ADDRESS:$PARTY_A_PAYMENT_SKEY" \
                         --expires $MINT_EXPIRES \
                         --token-provider "$PARTY_A_ADDRESS:$PARTY_A_PAYMENT_SKEY" \
                         --token-provider "$PARTY_B_ADDRESS:$PARTY_B_PAYMENT_SKEY" \
                         --submit 600 \
                         --out-file /dev/null

echo "Burning tokens issued by PARTY_B:"

marlowe-cli util burn --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$PARTY_B_ADDRESS:$PARTY_B_PAYMENT_SKEY" \
                      --expires $MINT_EXPIRES \
                      --token-provider "$PARTY_A_ADDRESS:$PARTY_A_PAYMENT_SKEY" \
                      --token-provider "$PARTY_B_ADDRESS:$PARTY_B_PAYMENT_SKEY" \
                      --submit 600 \
                      --out-file /dev/null


echo "Sending back funds:"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$PARTY_A_ADDRESS:$PARTY_A_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"

marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                  --send-all \
                                  --source-wallet-credentials "$PARTY_B_ADDRESS:$PARTY_B_PAYMENT_SKEY" \
                                  --submit 600 \
                                  --out-file /dev/null \
                                  "$FAUCET_ADDRESS"


echo "Here are the UTxOs at the second party $PARTY_A_NAME's address after the cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS"

echo "Here are the UTxOs at the second party $PARTY_B_NAME's address after the cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS"


