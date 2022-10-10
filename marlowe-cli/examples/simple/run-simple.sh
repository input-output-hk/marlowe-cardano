#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo "# Test of a Simple Contract"

echo "[This simple contract](../../../marlowe-contracts/src/Marlowe/Contracts/Trivial.hs) takes as deposit, waits for a notification, makes a payment, waits for another notification, and then closes the contract:"
echo
echo '```'
echo 'When'
echo '    [Case'
echo '        (Deposit'
echo '            (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")'
echo '            (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")'
echo '            (Token "" "")'
echo '            (Constant 12)'
echo '        )'
echo '        (When'
echo '            [Case'
echo '                (Notify TrueObs)'
echo '                (Pay'
echo '                    (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")'
echo '                    (Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"))'
echo '                    (Token "" "")'
echo '                    (Constant 5)'
echo '                    (When'
echo '                        [Case'
echo '                            (Notify TrueObs)'
echo '                            Close ]'
echo '                        2582625 Close '
echo '                    )'
echo '                )]'
echo '            2582624 Close '
echo '        )]'
echo '    2582623 Close '
echo '```'
echo
echo '![Simple Marlowe Contract](simple-0.png)'

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
echo "Signing and verification keys must be provided below for the bystander and party roles, or they will be created automatically: to do this, set the environment variables "'`'"BYSTANDER_PREFIX"'`'" and "'`'"PARTY_PREFIX"'`'" where they appear below."

echo "## Preliminaries"

: "${FAUCET_ADDRESS:?FAUCET_ADDRESS not set}"
: "${FAUCET_SKEY_FILE:?FAUCET_SKEY_FILE not set}"

echo "### Select Network"

: "${MAGIC:=2}"
echo "MAGIC=$MAGIC"

SLOT_LENGTH=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)

echo "### Participants"

echo "#### The Bystander"

echo "The bystander simply provides the minimum ada to be held in the contract while it is active."

BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_NAME="Christopher Marlowe"
BYSTANDER_PAYMENT_SKEY="$BYSTANDER_PREFIX".skey
BYSTANDER_PAYMENT_VKEY="$BYSTANDER_PREFIX".vkey

if [[ ! -e "$BYSTANDER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BYSTANDER_PAYMENT_SKEY"      \
                              --verification-key-file "$BYSTANDER_PAYMENT_VKEY"
fi

BYSTANDER_ADDRESS=$(
  cardano-cli address build --testnet-magic "$MAGIC"                                  \
                            --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY" \
)

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              --lovelace 50000000                       \
                              "$BYSTANDER_ADDRESS"

echo "The bystander $BYSTANDER_NAME is the minimum-ADA provider and has the address "'`'"$BYSTANDER_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                    \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                       --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                       --change-address "$BYSTANDER_ADDRESS"       \
                       --out-file /dev/null                        \
                       --submit=600                                \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS"

echo "We select a UTxO with sufficient funds to use in executing the contract."

TX_0_BYSTANDER=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BYSTANDER_ADDRESS"                      \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'
)

echo "$BYSTANDER_NAME will spend the UTxO "'`'"$TX_0_BYSTANDER"'`.'

echo "#### The Party"

echo "The party deposits and removes funds from the contract."

PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey

if [[ ! -e "$PARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_PAYMENT_VKEY"
fi

PARTY_ADDRESS=$(
  cardano-cli address build --testnet-magic "$MAGIC"                              \
                            --payment-verification-key-file "$PARTY_PAYMENT_VKEY" \
)

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 50000000                       \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$PARTY_ADDRESS"

echo "The party $PARTY_NAME has the address "'`'"$PARTY_ADDRESS"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS"

echo "We select the UTxO with the most funds to use in executing the contract."

TX_0_PARTY=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'
)

echo "$PARTY_NAME will spend the UTxO "'`'"$TX_0_PARTY"'`.'

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
# Add 1 millisecond so in the case of slot resolution larger than
# millisecond we test whether Marlowe time range is precomputed correctly.
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET+5))"
HOUR="$((3600*1000))"

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(((1000 * $(date -u +%s) - SLOT_OFFSET) / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "## The Contract"

echo "The contract has a minimum time and a timeout."

TIMEOUT_TIME="$((NOW+24*HOUR))"

echo "The contract will automatically close at $(date -u -R -d @$((TIMEOUT_TIME/1000)))."

echo "The contract also involves various payments."

MINIMUM_ADA=3000000
DEPOSIT_LOVELACE=12000000
WITHDRAWAL_LOVELACE=5000000
CLOSE_LOVELACE=$((DEPOSIT_LOVELACE-WITHDRAWAL_LOVELACE))

echo "The bystander $BYSTANDER_NAME will provide $MINIMUM_ADA lovelace during the contract's operation, so that it satisfies the minimmum-ADA requirement. The party $PARTY_NAME will deposit $DEPOSIT_LOVELACE lovelace at the start of the contract. They will wait until notified to withdraw $WITHDRAWAL_LOVELACE lovelace. After another notification, the party $PARTY_NAME will withdrawn the remaining $CLOSE_LOVELACE lovelace and the bystander $BYSTANDER_NAME will receive their $MINIMUM_ADA lovelace back. This is expressed in the Marlowe language [here](../../src/Language/Marlowe/CLI/Examples/Trivial.hs)."

echo "We create the contract for the previously specified parameters."

marlowe-cli template simple --bystander "$BYSTANDER_ADDRESS"             \
                            --minimum-ada "$MINIMUM_ADA"                 \
                            --party "$PARTY_ADDRESS"                     \
                            --deposit-lovelace "$DEPOSIT_LOVELACE"       \
                            --withdrawal-lovelace "$WITHDRAWAL_LOVELACE" \
                            --timeout "$TIMEOUT_TIME"                    \
                            --out-contract-file tx-1.contract            \
                            --out-state-file    tx-1.state

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA"

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require."

marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --merkleize                               \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "The bystander $BYSTANDER_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                    \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                        --tx-in "$TX_0_BYSTANDER"                   \
                        --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe             \
                        --change-address "$BYSTANDER_ADDRESS"       \
                        --out-file tx-1.raw                         \
                        --print-stats                               \
                        --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                    \
)

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the bystander $BYSTANDER_NAME in the transaction "'`'"$TX_1"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here is the UTxO at the bystander $BYSTANDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Make the Initial Deposit"

echo "First we compute the Marlowe input required to make the initial deposit by the party."

marlowe-cli run prepare --marlowe-file tx-1.marlowe              \
                        --deposit-account "$PARTY_ADDRESS"       \
                        --deposit-party "$PARTY_ADDRESS"         \
                        --deposit-amount "$DEPOSIT_LOVELACE"     \
                        --invalid-before "$NOW"                  \
                        --invalid-hereafter "$((NOW+4*HOUR))"    \
                        --out-file tx-2.marlowe                  \
                        --print-stats

echo "Now the party $PARTY_NAME submits the transaction along with their deposit:"

TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_0_PARTY"          \
                        --tx-in "$TX_0_PARTY"                     \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract received the deposit of $DEPOSIT_LOVELACE lovelace from the party $PARTY_NAME in the transaction "'`'"$TX_2"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. Make the First Withdrawal"

echo "First we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --notify                              \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats

echo "Now the party $PARTY_NAME can submit a transaction to withdraw funds:"

TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_2"#0              \
                        --tx-in "$TX_2"#0                         \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The contract made a payment of $WITHDRAWAL_LOVELACE lovelace to the party $PARTY_NAME in the transaction "'`'"$TX_3"'`'". Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. Close the contract"

echo "As in the third transaction, we compute the input for the contract to transition forward."

marlowe-cli run prepare --marlowe-file tx-3.marlowe           \
                        --notify                              \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-4.marlowe               \
                        --print-stats

echo "Now the party $PARTY_NAME can submit a transaction to close the contract and disperse the remaining funds:"

TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-3.marlowe            \
                        --tx-in-marlowe "$TX_3"#1                 \
                        --tx-in-collateral "$TX_3"#0              \
                        --tx-in "$TX_3"#0                         \
                        --tx-in "$TX_3"#2                         \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-4.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-4.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)

echo "The closing of the contract paid $CLOSE_LOVELACE lovelace to the the party $PARTY_NAME and $MINIMUM_ADA lovelace to the bystander $BYSTANDER_NAME in the transaction "'`'"$TX_4"'`'". There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "Here is the UTxO at the bystander $BYSTANDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "Here is the UTxO at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "## Clean Up"

cleanup() {
  SRC_SKEY="$1"
  SRC_ADDR="$2"
  DEST_ADDR="$3"

  echo "Sending back funds:"

  marlowe-cli -- util fund-address  --testnet-magic "$MAGIC" \
                                    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                    --send-all \
                                    --source-wallet-credentials "$SRC_ADDR:$SRC_SKEY" \
                                    --submit 600 \
                                    --out-file /dev/null \
                                    "$DEST_ADDR"

  cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SRC_ADDR"
}

cleanup "$BYSTANDER_PAYMENT_SKEY" "$BYSTANDER_ADDRESS" "$FAUCET_ADDRESS"

cleanup "$PARTY_PAYMENT_SKEY" "$PARTY_ADDRESS" "$FAUCET_ADDRESS"

echo "## AFTER CLEANUP"

echo "Here is the UTxO at the bystander $BYSTANDER_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

echo "Here is the UTxO at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"

