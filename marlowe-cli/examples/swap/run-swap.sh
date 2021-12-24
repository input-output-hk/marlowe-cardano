#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -e

echo "# Test of a Swap Contract"

echo "[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties."

echo "## Prerequisites"

echo "The environment variable "'`CARDANO_NODE_SOCKET_PATH`'" must be set to the path to the cardano node's socket."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo '* xargs'
echo
echo 'Signing and verification keys must be provided below for the two parties: to do this, set the environment variables `PARTY_A_PREFIX` and `PARTY_B_PREFIX` where they appear below.'
echo
echo "The two parties' wallets much have exactly one UTxO with the token they want to swap and at least one UTxO without tokens."

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

echo "#### The First Party"

PARTY_A_PREFIX="$TREASURY/john-fletcher"
PARTY_A_NAME="John Fletcher"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey
PARTY_A_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" \
)
PARTY_A_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY"
)

echo "The first party $PARTY_A_NAME is the minimum-ADA provider and has the address "'`'"$PARTY_A_ADDRESS"'`'" and public-key hash "'`'"$PARTY_A_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with exactly one type of native token."

TX_0_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_A_ADDRESS"                                                             \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$PARTY_A_ADDRESS"                                                                                                    \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)

echo "$PARTY_A_NAME will spend the UTxOs "'`'"$TX_0_A_ADA"'`'" and "'`'"$TX_0_A_TOKEN"'`'". They will trade $AMOUNT_A of "'`'"$TOKEN_A"'`.'

echo "### The Second Party"

PARTY_B_PREFIX="$TREASURY/thomas-middleton"
PARTY_B_NAME="Thomas Middleton"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey
PARTY_B_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" \
)
PARTY_B_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY"
)

echo "The second party $PARTY_B_NAME has the address "'`'"$PARTY_B_BDDRESS"'`'" and public-key hash "'`'"$PARTY_B_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with exactly one type of native token."

TX_0_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_B_ADDRESS"                                                             \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$PARTY_B_ADDRESS"                                                                                                    \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json | head -n 1)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json | head -n 1)

echo "$PARTY_B_NAME will spend the UTxOs "'`'"$TX_0_B_ADA"'`'" and "'`'"$TX_0_B_TOKEN"'`'". They will trade $AMOUNT_B of "'`'"$TOKEN_B"'`.'

echo "### Validator Script and Address"

echo "The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require."

CONTRACT_ADDRESS=$(
marlowe-cli contract address "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli contract validator "${MAGIC[@]}"                \
                               --slot-length "$SLOT_LENGTH" \
                               --slot-offset "$SLOT_OFFSET" \
                               --out-file swap.plutus

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(($(date -u +%s) - $SLOT_OFFSET / $SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

echo "The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party $PARTY_A_NAME will swap $AMOUNT_A of "'`'"$TOKEN_A"'`'" for $AMOUNT_B of "'`'"$TOKEN_B"'`'" from the second party $PARTY_B_NAME."

MINIMUM_ADA=3000000

PARTY_A_TIMEOUT=$(($TIP+12*3600))
PARTY_B_TIMEOUT=$(($TIP+24*3600))

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

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
                          --out-file tx-1.marlowe

echo 'We extract the initial state and full contract from the `.marlowe`file that contains comprehensive information.'

jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract

echo "For each transaction, we construct the output datum. Here is its size and hash:"

marlowe-cli contract datum --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.datum

echo "The first party $PARTY_A_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace requiredd for the contract's initial state. Submitting with the "'`--print-stats`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli transaction create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --script-address "$CONTRACT_ADDRESS"      \
                               --tx-in "$TX_0_A_ADA"                     \
                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                               --tx-out-datum-file tx-1.datum            \
                               --tx-out-marlowe "$MINIMUM_ADA"           \
                               --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA"  \
                               --change-address "$PARTY_A_ADDRESS"       \
                               --out-file tx-1.raw                       \
                               --print-stats                             \
                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the first party $PARTY_A_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. First Party Deposits Tokens into the Contract."

echo "First we compute the Marlowe input required to deposit the tokens."

marlowe-cli input deposit --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_A"                 \
                          --deposit-amount "$AMOUNT_A"               \
                          --out-file tx-2.input

echo "Next we compute the transition caused by that input to the contract."

marlowe-cli run compute --contract-file tx-1.contract          \
                        --state-file    tx-1.state             \
                        --input-file    tx-2.input             \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-2.marlowe                \
                        --print-stats

echo "As in the first transaction, we compute the new state and contract."

jq '.state'    tx-2.marlowe > tx-2.state
jq '.contract' tx-2.marlowe > tx-2.contract

echo "Because this transaction spends from the script address, it also needs a redeemer:"

marlowe-cli contract redeemer --input-file tx-2.input    \
                              --out-file   tx-2.redeemer

echo "As in the first transaction, we compute the datum and its hash:"

marlowe-cli contract datum --contract-file tx-2.contract \
                           --state-file    tx-2.state    \
                           --out-file      tx-2.datum

echo "Now the first party $PARTY_A_NAME submits the transaction that deposits their tokens."

TX_2=$(
marlowe-cli transaction advance "${MAGIC[@]}"                                      \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH"          \
                                --script-address "$CONTRACT_ADDRESS"               \
                                --tx-in-marlowe "$TX_1"#1                          \
                                --tx-in-script-file swap.plutus                    \
                                --tx-in-datum-file tx-1.datum                      \
                                --tx-in-redeemer-file tx-2.redeemer                \
                                --tx-in "$TX_0_A_TOKEN"                            \
                                --tx-in "$TX_1"#0                                  \
                                --tx-in "$TX_1"#2                                  \
                                --tx-in-collateral "$TX_1"#0                       \
                                --required-signer "$PARTY_A_PAYMENT_SKEY"          \
                                --tx-out-marlowe "$MINIMUM_ADA+$AMOUNT_A $TOKEN_A" \
                                --tx-out-datum-file tx-2.datum                     \
                                --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA"           \
                                --change-address "$PARTY_A_ADDRESS"                \
                                --invalid-before "$TIP"                            \
                                --invalid-hereafter "$(($TIP+4*3600))"             \
                                --out-file tx-2.raw                                \
                                --print-stats                                      \
                                --submit=600                                       \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                   \
)

echo "The contract received the deposit of $AMOUNT_A "'`'"$TOKEN_A"'`'" in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap."

echo "First we compute the input for the contract to transition forward."

marlowe-cli input deposit --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_B"                 \
                          --deposit-amount "$AMOUNT_B"               \
                          --out-file tx-3.input

echo "As in the second transaction we compute the contract's transition, its new state, and the redeemer. Because the contract is being closed, no new datum need be computed."

marlowe-cli run compute --contract-file tx-2.contract          \
                        --state-file    tx-2.state             \
                        --input-file    tx-3.input             \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-3.marlowe                \
                        --print-stats
jq '.state'    tx-3.marlowe > tx-3.state
jq '.contract' tx-3.marlowe > tx-3.contract
marlowe-cli contract redeemer --input-file tx-3.input    \
                              --out-file   tx-3.redeemer
marlowe-cli contract datum --contract-file tx-3.contract \
                           --state-file    tx-3.state    \
                           --out-file      tx-3.datum

echo "Now the second party $PARTY_B_NAME can submit a transaction that deposits their tokens and completes the swap."

TX_3=$(
marlowe-cli transaction close "${MAGIC[@]}"                                               \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                              --tx-in-marlowe "$TX_2"#1                                   \
                              --tx-in-script-file swap.plutus                             \
                              --tx-in-datum-file tx-2.datum                               \
                              --tx-in-redeemer-file tx-3.redeemer                         \
                              --tx-in "$TX_0_B_ADA"                                       \
                              --tx-in "$TX_0_B_TOKEN"                                     \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"                   \
                              --tx-in-collateral "$TX_0_B_ADA"                            \
                              --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA+$AMOUNT_B $TOKEN_B" \
                              --tx-out "$PARTY_B_ADDRESS+$MINIMUM_ADA+$AMOUNT_A $TOKEN_A" \
                              --change-address "$PARTY_B_ADDRESS"                         \
                              --invalid-before "$TIP"                                     \
                              --invalid-hereafter "$(($TIP+4*3600))"                      \
                              --out-file tx-3.raw                                         \
                              --print-stats                                               \
                              --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                          \
)

echo "The closing of the contract paid $AMOUNT_B "'`'"$TOKEN_B"'`'" to the first party $PARTY_A_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract, and it paid $AMOUNT_A "'`'"$TOKEN_A"'`'" to the second party $PARTY_B_NAME in the transaction "'`'"$TX_3"'`'". There is no UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here are the UTxOs at the first party $PARTY_A_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p;/$TX_3/p"

echo "Here are the UTxOs at the second party $PARTY_B_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Clean Up"

echo "It's convenient to consolidate the UTxOs for the first party."

marlowe-cli transaction simple "${MAGIC[@]}"                                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                               --tx-in "$TX_2"#0                                           \
                               --tx-in "$TX_2"#2                                           \
                               --tx-in "$TX_3"#1                                           \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"                   \
                               --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA+$AMOUNT_B $TOKEN_B" \
                               --change-address "$PARTY_A_ADDRESS"                         \
                               --out-file tx-4.raw                                         \
                               --submit=600                                                \
> /dev/null

