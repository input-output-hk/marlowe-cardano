#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -e

echo "# Test of a Covered Call Contract"

echo "[This option contract](../../../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option."

echo "## Prerequisites"

echo "The environment variable "'`'"CARDANO_NODE_SOCKET_PATH"'`'" must be set to the path to the cardano node's socket."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo
echo "Signing and verification keys must be provided below for the two parties: to do this, set the environment variables "'`'"ISSUER_PREFIX"'`'" and "'`'"COUNTERPARTY_PREFIX"'`'" where they appear below."
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
  SLOT_OFFSET=1644929640000
fi

echo "#### The Issuer"

ISSUER_PREFIX="$TREASURY/john-fletcher"
ISSUER_NAME="John Fletcher"
ISSUER_PAYMENT_SKEY="$ISSUER_PREFIX".skey
ISSUER_PAYMENT_VKEY="$ISSUER_PREFIX".vkey
ISSUER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$ISSUER_PAYMENT_VKEY" \
)
ISSUER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$ISSUER_PAYMENT_VKEY"
)

echo "The issuer $ISSUER_NAME is the minimum-ADA provider and has the address "'`'"$ISSUER_ADDRESS"'`'" and public-key hash "'`'"$ISSUER_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ISSUER_PAYMENT_SKEY"  \
                       --change-address "$ISSUER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with exactly one type of native token."

TX_0_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$ISSUER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$ISSUER_ADDRESS"                                                                                                     \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[1].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1 | sed -e 's/\(.*\)/\U\1/' | basenc --decode --base16)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)

echo "$ISSUER_NAME will spend the UTxOs "'`'"$TX_0_A_ADA"'`'" and "'`'"$TX_0_A_TOKEN"'`'". They will trade $AMOUNT_A of "'`'"$TOKEN_A"'`.'

echo "### The Counter-party"

COUNTERPARTY_PREFIX="$TREASURY/thomas-kyd"
COUNTERPARTY_NAME="Thomas Kyd"
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey
COUNTERPARTY_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                                \
                            --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" \
)
COUNTERPARTY_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
)

echo "The counter party $COUNTERPARTY_NAME has the address "'`'"$COUNTERPARTY_ADDRESS"'`'" and public-key hash "'`'"$COUNTERPARTY_PUBKEYHASH"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean "${MAGIC[@]}"                                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS"

echo "We select the UTxO with the most ADA and another UTxO with exactly one type of native token."

TX_0_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$COUNTERPARTY_ADDRESS"                                                                                               \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[1].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json | head -n 1 | sed -e 's/\(.*\)/\U\1/' | basenc --decode --base16)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json | head -n 1)

echo "$COUNTERPARTY_NAME will spend the UTxOs "'`'"$TX_0_B_ADA"'`'" and "'`'"$TX_0_B_TOKEN"'`'". They will trade $AMOUNT_B of "'`'"$TOKEN_B"'`.'

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
SECOND="1000"

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(($(date -u +%s) - SLOT_OFFSET / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

MINIMUM_ADA=3000000

ISSUE_TIMEOUT=$((NOW+2*MINUTE-1*SECOND))
MATURITY_TIMEOUT=$((NOW+2*MINUTE))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))

echo "The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer $ISSUER_NAME will put $AMOUNT_A of "'`'"$TOKEN_A"'`'" before $(date -u -R -d @$((ISSUE_TIMEOUT/1000))) into the contract and if the counter-party $COUNTERPARTY_NAME exercises the option for $AMOUNT_B of "'`'"$TOKEN_B"'`'" after $(date -u -R -d @$((MATURITY_TIMEOUT/1000))) and before $(date -u -R -d @$((SETTLEMENT_TIMEOUT/1000)))."

echo "We create the contract for the previously specified parameters."

marlowe-cli template coveredCall --minimum-ada "$MINIMUM_ADA"                  \
                                 --issuer "PK=$ISSUER_PUBKEYHASH"              \
                                 --counter-party "PK=$COUNTERPARTY_PUBKEYHASH" \
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

marlowe-cli run initialize "${MAGIC[@]}"                 \
                           --slot-length "$SLOT_LENGTH"  \
                           --slot-offset "$SLOT_OFFSET"  \
                           --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.marlowe  \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "The issuer $ISSUER_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
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

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the issuer $ISSUER in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here are the UTxOs at the issuer $ISSUER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. The issuer deposits Tokens into the Contract."

echo "First we compute the Marlowe input required to deposit the tokens."

marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "PK=$ISSUER_PUBKEYHASH" \
                        --deposit-party "PK=$ISSUER_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                \
                        --deposit-amount "$AMOUNT_A"              \
                        --invalid-before "$((NOW-300000))"        \
                        --invalid-hereafter "$((NOW+2*MINUTE-2*SECOND))"     \
                        --out-file tx-2.marlowe                   \
                        --print-stats

echo "Now the issuer $ISSUER_NAME submits the transaction that deposits their tokens."

TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
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

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the issuer $ISSUER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. Wait until expiry (2 minutes) and advance contract"

sleep 2m

TX_1_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$ISSUER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --invalid-before "$((NOW+2*MINUTE))"  \
                        --invalid-hereafter "$((NOW+8*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats

TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_1_A_ADA"          \
                        --tx-in "$TX_1_A_ADA"                     \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "## Transaction 4. The Counter-Party chooses to exercise the option"

marlowe-cli run prepare --marlowe-file tx-3.marlowe                   \
                        --choice-name "Exercise Call"                 \
                        --choice-party "PK=$COUNTERPARTY_PUBKEYHASH"  \
                        --choice-number 1                             \
                        --invalid-before "$((NOW+2*MINUTE+1*SECOND))" \
                        --invalid-hereafter "$((NOW+8*HOUR))"         \
                        --out-file tx-4.marlowe                       \
                        --print-stats


TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-3.marlowe                 \
                        --tx-in-marlowe "$TX_3"#1                      \
                        --tx-in-collateral "$TX_0_B_ADA"               \
                        --tx-in "$TX_0_B_ADA"                          \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-4.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-4.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)

echo "## Transaction 5. The Counter-Party Deposits their Tokens."

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-4.marlowe                     \
                        --deposit-account "PK=$COUNTERPARTY_PUBKEYHASH" \
                        --deposit-party "PK=$COUNTERPARTY_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$((NOW+2*MINUTE+1*SECOND))"   \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-5.marlowe                         \
                        --print-stats

echo "Now the counter-party $COUNTERPARTY_NAME can submit a transaction that deposits their tokens."

TX_4_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)

TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-4.marlowe                 \
                        --tx-in-marlowe "$TX_4"#1                      \
                        --tx-in-collateral "$TX_4_B_ADA"               \
                        --tx-in "$TX_4_B_ADA"                          \
                        --tx-in "$TX_0_B_TOKEN"                        \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-5.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-5.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)

echo "The closing of the contract paid $AMOUNT_B "'`'"$TOKEN_B"'`'" to the first party $ISSUER_NAME, along with the minimum ADA $MINIMUM_ADA lovelace that they deposited when creating the contract, and it paid $AMOUNT_A "'`'"$TOKEN_A"'`'" to the counter party $COUNTERPARTY_NAME in the transaction "'`'"$TX_5"'`'". There is no UTxO at the contract address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_5/p"

echo "Here are the UTxOs at the issuer party $ISSUER_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here are the UTxOs at the counter party $COUNTERPARTY_NAME's address:"

cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

