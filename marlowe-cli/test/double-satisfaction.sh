#!/usr/bin/env bash


# Test of double-satisfaction from Marlowe scripts.
#
# This script creates two running contracts that are ready to close. It then
# runs a transaction that simultaneously closes both contracts, stealing ADA
# from one party.
#
# Prerequisites:
#   1. Set the paths in PARTY_A_PREFIX and PARTY_B_PREFIX below.
#   2. Set CARDANO_NODE_SOCKET_PATH.
#   3. The following tools must be on the PATH:
#      *  marlowe-cli
#      *  cardano-cli
#      *  jq
#      *  sed
#      *  xargs


set -ev


# Because of the transaction size, this test must be run on the private Marlowe
# testnet.

MAGIC=(--testnet-magic 1564)
SLOT_LENGTH=1000
SLOT_OFFSET=1638215277000


# Configure the first party.

PARTY_A_PREFIX="$TREASURY/francis-beaumont"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey
PARTY_A_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" \
)
PARTY_A_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY"
)

echo "The first party has the address $PARTY_A_ADDRESS and the public-key hash $PARTY_A_PUBKEYHASH. They have the following UTxOs in their wallet:"
echo
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS"

echo "We select the UTxO with the most funds to use in executing the contract."
TX_0_PARTY_A=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$PARTY_A_ADDRESS"                    \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo "The first party will spend the UTxO $TX_0_PARTY_A."


# Configure the second party.

PARTY_B_PREFIX="$TREASURY/christopher-marlowe"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey
PARTY_B_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" \
)
PARTY_B_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY"
)

echo "The second party has the address $PARTY_B_ADDRESS and public-key hash $PARTY_B_PUBKEYHASH. They have the following UTxOs in their wallet:"
echo
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS"

echo "We select the UTxO with the most funds to use in executing the contract."
TX_0_PARTY_B=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$PARTY_B_ADDRESS"                    \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
echo "The second party will spend the UTxO $TX_0_PARTY_B."


# Configure the validator.

CONTRACT_ADDRESS=$(
marlowe-cli export-address "${MAGIC[@]}" \
            --slot-length "$SLOT_LENGTH" \
            --slot-offset "$SLOT_OFFSET" \
)

marlowe-cli export-validator "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
                             --out-file marlowe.plutus    \
                             --print-stats


# Find the protocol parameters and the tip.

cardano-cli query  protocol-parameters "${MAGIC[@]}" --out-file private.protocol

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')

MINIMUM_SLOT="$TIP"
TIMEOUT_SLOT="$(($TIP+4*3600))"


# Transaction 1. Configure the first contract.

# In the first contract, the first party has 25 ADA their account and the second
# party has 3 ADA in theirs. We ignore the prior operation of the contract and
# assume without loss of generality that the contract has reached "close".

cat > tx-1.state << EOI
{
  "choices": [],
  "accounts": [
    [
      [ { "pk_hash": "$PARTY_A_PUBKEYHASH" }, { "currency_symbol": "", "token_name": "" } ],
      25000000
    ],
    [
      [ { "pk_hash": "$PARTY_B_PUBKEYHASH" },
        { "currency_symbol": "", "token_name": "" } ],
      3000000
    ]
  ],
  "minSlot": $TIP,
  "boundValues": []
}
EOI

cat > tx-1.contract << EOI
"close"
EOI

marlowe-cli export-datum --contract-file tx-1.contract \
                         --state-file    tx-1.state    \
                         --out-file      tx-1.datum

TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_0_PARTY_A"                   \
                               --change-address "$PARTY_A_ADDRESS"       \
                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"      \
                               --tx-out-datum-file tx-1.datum            \
                               --tx-out-marlowe 28000000                 \
                               --out-file tx-1.raw                       \
                               --print-stats                             \
                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)
echo "TxId $TX_1"


# Transaction 2. Configure the second contract.

# In the second contract, the first party has 20 ADA their account and the
# second party has 4 ADA in theirs. We ignore the prior operation of the
# contract and assume without loss of generality that the contract has reached
# "close".

cat > tx-2.state << EOI
{
  "choices": [],
  "accounts": [
    [
      [ { "pk_hash": "$PARTY_A_PUBKEYHASH" }, { "currency_symbol": "", "token_name": "" } ],
      20000000
    ],
    [
      [ { "pk_hash": "$PARTY_B_PUBKEYHASH" },
        { "currency_symbol": "", "token_name": "" } ],
      4000000
    ]
  ],
  "minSlot": $TIP,
  "boundValues": []
}
EOI

cat > tx-2.contract << EOI
"close"
EOI

marlowe-cli export-datum --contract-file tx-2.contract \
                         --state-file    tx-2.state    \
                         --out-file      tx-2.datum

TX_2=$(
marlowe-cli transaction-create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_0_PARTY_B"                   \
                               --change-address "$PARTY_B_ADDRESS"       \
                               --required-signer "$PARTY_B_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"      \
                               --tx-out-datum-file tx-2.datum            \
                               --tx-out-marlowe 24000000                 \
                               --out-file tx-2.raw                       \
                               --print-stats                             \
                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)
echo "TxId $TX_2"


# Transaction 3. The second party steals 20 ADA from the first party.

# Each contract is ready to close. When the first contract closes, it should
# pay 25 ADA to the first party and 3 ADA to the second party. When the seconnd
# contract closes, it should pay 20 ADA to the first party and 4 ADA to the
# second party. We can verify this by checking the transactions for running the
# two contracts separately and together.

# Compute the redeemers for the two UTxOs at the contract address:
marlowe-cli export-redeemer --out-file tx-2-first.redeemer
marlowe-cli export-redeemer --out-file tx-2-second.redeemer

# Perform a dry-run of redeeming the first contact: the first party receives
# their 25 ADA and the second party receives their 3 ADA. If the following
# command prints the transaction ID, that indicates that the transaction would
# run without error. (It doesn't matter which party submits this transaction.)
marlowe-cli transaction-close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_1"#1                  \
                              --tx-in-script-file marlowe.plutus         \
                              --tx-in-datum-file tx-1.datum              \
                              --tx-in-redeemer-file tx-2-first.redeemer  \
                              --tx-in "$TX_2"#0                          \
                              --tx-in-collateral "$TX_2"#0               \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"  \
                              --tx-out "$PARTY_A_ADDRESS+25000000"       \
                              --tx-out "$PARTY_B_ADDRESS+3000000"        \
                              --change-address "$PARTY_B_ADDRESS"        \
                              --invalid-before "$MINIMUM_SLOT"           \
                              --invalid-hereafter "$TIMEOUT_SLOT"        \
                              --out-file tx-2.raw                        \
                              --print-stats

# Perform a dry-run of redeeming the second contact: the first party receives
# their 20 ADA and the second party receives their 4 ADA. If the following
# command prints the transaction ID, that indicates that the transaction would
# run without error. (It doesn't matter which party submits this transaction.)
marlowe-cli transaction-close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_2"#1                  \
                              --tx-in-script-file marlowe.plutus         \
                              --tx-in-datum-file tx-2.datum              \
                              --tx-in-redeemer-file tx-2-second.redeemer \
                              --tx-in "$TX_2"#0                          \
                              --tx-in-collateral "$TX_2"#0               \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"  \
                              --tx-out "$PARTY_A_ADDRESS+20000000"       \
                              --tx-out "$PARTY_B_ADDRESS+4000000"        \
                              --change-address "$PARTY_B_ADDRESS"        \
                              --invalid-before "$MINIMUM_SLOT"           \
                              --invalid-hereafter "$TIMEOUT_SLOT"        \
                              --out-file tx-2.raw                        \
                              --print-stats

# Perform a dry-run of redeeming both contracts at once: the first party
# receives their 25+20=45 ADA and the second party receives their 3+4=7 ADA. If
# the following commands succeeds, that indicates that the transaction would run
# without error. (It doesn't matter which party submits this transaction.)
cardano-cli transaction build "${MAGIC[@]}" --alonzo-era                   \
                              --protocol-params-file private.protocol      \
                              --tx-in "$TX_1"#1                            \
                                --tx-in-script-file marlowe.plutus         \
                                --tx-in-datum-file tx-1.datum              \
                                --tx-in-redeemer-file tx-2-first.redeemer  \
                              --tx-in "$TX_2"#1                            \
                                --tx-in-script-file marlowe.plutus         \
                                --tx-in-datum-file tx-2.datum              \
                                --tx-in-redeemer-file tx-2-second.redeemer \
                              --tx-in "$TX_2"#0                            \
                              --tx-in-collateral "$TX_2"#0                 \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"    \
                              --tx-out "$PARTY_A_ADDRESS+45000000"         \
                              --tx-out "$PARTY_B_ADDRESS+7000000"          \
                              --change-address "$PARTY_B_ADDRESS"          \
                              --invalid-before "$MINIMUM_SLOT"             \
                              --invalid-hereafter "$TIMEOUT_SLOT"          \
                              --out-file tx-2.raw

# Perform a dry-run of redeeming both contracts at once: the first party only
# receives their 25 ADA from the first contract and the second party steals
# the first party's 20 ADA from the second contract because the second validator
# sees that >= 20 ADA is sent to the first party--i.e., this is a double-spend
# attack; the the second party receives 20+3+4=27 ADA put. If the following
# commands succeeds, that indicates that the transaction would run without
# error. (It doesn't matter which party submits this transaction, but the second
# party submits it because they are the thief.)
cardano-cli transaction build "${MAGIC[@]}" --alonzo-era                   \
                              --protocol-params-file private.protocol      \
                              --tx-in "$TX_1"#1                            \
                                --tx-in-script-file marlowe.plutus         \
                                --tx-in-datum-file tx-1.datum              \
                                --tx-in-redeemer-file tx-2-first.redeemer  \
                              --tx-in "$TX_2"#1                            \
                                --tx-in-script-file marlowe.plutus         \
                                --tx-in-datum-file tx-2.datum              \
                                --tx-in-redeemer-file tx-2-second.redeemer \
                              --tx-in "$TX_2"#0                            \
                              --tx-in-collateral "$TX_2"#0                 \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"    \
                              --tx-out "$PARTY_A_ADDRESS+25000000"         \
                              --tx-out "$PARTY_B_ADDRESS+27000000"         \
                              --change-address "$PARTY_B_ADDRESS"          \
                              --invalid-before "$MINIMUM_SLOT"             \
                              --invalid-hereafter "$TIMEOUT_SLOT"          \
                              --out-file tx-2.raw

# Now sign an submit the transaction, just to make sure that the theft really
# can occur.
TX_3=$(
marlowe-cli transaction-submit "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-body-file tx-2.raw                   \
                               --required-signer "$PARTY_B_PAYMENT_SKEY" \
                               --timeout 600                             \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)
echo "TxId $TX_3"

echo "There is no UTxO at the contract address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here is the UTxO at the first party's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"

echo "Here is the UTxO at the second party's address:"
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"


# Clean up wallets

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$PARTY_A_ADDRESS"       \
                                               --out-file tx-3.raw                       \
                                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                                               --submit=600                              \
> /dev/null

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$PARTY_B_ADDRESS"       \
                                               --out-file tx-4.raw                       \
                                               --required-signer "$PARTY_B_PAYMENT_SKEY" \
                                               --submit=600                              \
> /dev/null
