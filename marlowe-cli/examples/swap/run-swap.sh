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

echo "### Select Network"

if [[ -z "$MAGIC" ]]
then
  MAGIC=(--testnet-magic 1567)
fi

SLOT_LENGTH=$(marlowe-cli util slotting "${MAGIC[@]}" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting "${MAGIC[@]}" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
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
PARTY_A_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" )
PARTY_A_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY")

echo "Fund the first party's address."

marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$PARTY_A_ADDRESS"

echo "The first party mints their tokens for the swap."

MINT_EXPIRES=$((TIP + 1000000))

TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_A_PAYMENT_SKEY" \
                      --change-address  "$PARTY_A_ADDRESS"      \
                      --count "$AMOUNT_A"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"

echo "The first party $PARTY_A_NAME is the minimum-ADA provider and has the address "'`'"$PARTY_A_ADDRESS"'`'" and public-key hash "'`'"$PARTY_A_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_A_PAYMENT_SKEY" \
                       --change-address "$PARTY_A_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_A_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
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
PARTY_B_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" )
PARTY_B_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY")

echo "Fund the second party's address."

marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$PARTY_B_ADDRESS"

echo "The second party mints their tokens for the swap."

TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_B_PAYMENT_SKEY" \
                      --change-address  "$PARTY_B_ADDRESS"      \
                      --count "$AMOUNT_B"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_B"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"

echo "The second party $PARTY_B_NAME has the address "'`'"$PARTY_B_ADDRESS"'`'" and public-key hash "'`'"$PARTY_B_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_B_PAYMENT_SKEY" \
                       --change-address "$PARTY_B_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with the newly minted native token."

TX_0_B_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
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
                          --a-party "PK=$PARTY_A_PUBKEYHASH" \
                          --a-token "$TOKEN_A"               \
                          --a-amount "$AMOUNT_A"             \
                          --a-timeout "$PARTY_A_TIMEOUT"     \
                          --b-party "PK=$PARTY_B_PUBKEYHASH" \
                          --b-token "$TOKEN_B"               \
                          --b-amount "$AMOUNT_B"             \
                          --b-timeout "$PARTY_B_TIMEOUT"     \
                          --out-contract-file tx-1.contract  \
                          --out-state-file    tx-1.state

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require."

marlowe-cli run initialize "${MAGIC[@]}"                             \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "The first party $PARTY_A_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
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

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. First Party Deposits Tokens into the Contract."

echo "First we compute the Marlowe input required to deposit the tokens."

marlowe-cli run prepare --marlowe-file tx-1.marlowe                \
                        --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                 \
                        --deposit-amount "$AMOUNT_A"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-2.marlowe                    \
                        --print-stats

echo "Now the first party $PARTY_A_NAME submits the transaction that deposits their tokens."

TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
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

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                 \
                        --deposit-amount "$AMOUNT_B"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-3.marlowe                    \
                        --print-stats

echo "Now the second party $PARTY_B_NAME can submit a transaction that deposits their tokens and completes the swap."

TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
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

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here are the UTxOs at the second party $PARTY_B_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "## Clean Up"

FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8

marlowe-cli transaction simple "${MAGIC[@]}"                                          \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_2"#0                                      \
                               --tx-in "$TX_3"#2                                      \
                               --tx-out "$PARTY_B_ADDRESS+1500000+$AMOUNT_B $TOKEN_B" \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"              \
                               --change-address "$PARTY_A_ADDRESS"                    \
                               --out-file /dev/null                                   \
                               --submit 600

marlowe-cli transaction simple "${MAGIC[@]}"                                          \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_3"#0                                      \
                               --tx-in "$TX_3"#1                                      \
                               --tx-out "$PARTY_A_ADDRESS+1500000+$AMOUNT_A $TOKEN_A" \
                               --required-signer "$PARTY_B_PAYMENT_SKEY"              \
                               --change-address "$PARTY_B_ADDRESS"                    \
                               --out-file /dev/null                                   \
                               --submit 600

marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_A_PAYMENT_SKEY" \
                      --change-address  "$PARTY_A_ADDRESS"      \
                      --count "-$AMOUNT_A"                      \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"

marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_B_PAYMENT_SKEY" \
                      --change-address  "$PARTY_B_ADDRESS"      \
                      --count "-$AMOUNT_B"                      \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_B"

TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600

TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)

marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$PARTY_B_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
