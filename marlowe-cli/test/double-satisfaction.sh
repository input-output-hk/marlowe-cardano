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


: "${FAUCET_ADDRESS:?FAUCET_ADDRESS not set}"
: "${FAUCET_SKEY_FILE:?FAUCET_SKEY_FILE not set}"


set -ev


: "${MAGIC:="2"}"
echo "MAGIC=$MAGIC"


# Configure the first party.

PARTY_A_PREFIX="$TREASURY/francis-beaumont"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey

# Create the first party's keys, if necessary.

if [[ ! -e "$PARTY_A_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_A_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_A_PAYMENT_VKEY"
fi
PARTY_A_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" )


# Fund the first party's address.

marlowe-cli util fund-address \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --out-file /dev/null \
  --submit 600 \
  --lovelace 100000000 \
  --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
  "$PARTY_A_ADDRESS"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_A_PAYMENT_SKEY" \
                       --change-address "$PARTY_A_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS"


# We select the UTxO with sufficient funds to use in executing the contract.

TX_0_PARTY_A=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 50000000                  \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)


# Configure the second party.

PARTY_B_PREFIX="$TREASURY/christopher-marlowe"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey


# Create the second party's keys, if necessary.

if [[ ! -e "$PARTY_B_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_B_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_B_PAYMENT_VKEY"
fi
PARTY_B_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" )


# Fund the second party's address.

marlowe-cli util fund-address \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --out-file /dev/null \
  --submit 600 \
  --lovelace 100000000 \
  --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
  "$PARTY_B_ADDRESS"

marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_B_PAYMENT_SKEY" \
                       --change-address "$PARTY_B_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS"


# We select the UTxO with sufficient funds to use in executing the contract.

TX_0_PARTY_B=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 50000000                  \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
echo "TX_0_PARTY_B: $TX_0_PARTY_B"


# Configure the validator.

CONTRACT_ADDRESS=$(marlowe-cli contract address --testnet-magic "$MAGIC")

marlowe-cli contract validator --testnet-magic "$MAGIC"  \
                               --out-file marlowe.plutus \
                               --print-stats


# Find the protocol parameters and the tip.

cardano-cli query  protocol-parameters --testnet-magic "$MAGIC" --out-file private.protocol

TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')

MINIMUM_SLOT="$TIP"
TIMEOUT_SLOT="$((TIP+4*3600))"


# Transaction 1. Configure the first contract.

# In the first contract, the first party has 25 ADA their account and the second
# party has 3 ADA in theirs. We ignore the prior operation of the contract and
# assume without loss of generality that the contract has reached "close".

cat > tx-1.state << EOI
{
  "choices": [],
  "accounts": [
    [
      [ { "address": "$PARTY_A_ADDRESS" }, { "currency_symbol": "", "token_name": "" } ],
      25000000
    ],
    [
      [ { "address": "$PARTY_B_ADDRESS" },
        { "currency_symbol": "", "token_name": "" } ],
      3000000
    ]
  ],
  "minTime": 1,
  "boundValues": []
}
EOI

cat > tx-1.contract << EOI
"close"
EOI

marlowe-cli contract datum --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.datum

TX_1=$(
marlowe-cli transaction create --testnet-magic "$MAGIC"                  \
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
      [ { "address": "$PARTY_A_ADDRESS" }, { "currency_symbol": "", "token_name": "" } ],
      20000000
    ],
    [
      [ { "address": "$PARTY_B_ADDRESS" },
        { "currency_symbol": "", "token_name": "" } ],
      4000000
    ]
  ],
  "minTime": 1,
  "boundValues": []
}
EOI

cat > tx-2.contract << EOI
"close"
EOI

marlowe-cli contract datum --contract-file tx-2.contract  \
                           --state-file    tx-2.state     \
                           --out-file      tx-2.datum

TX_2=$(
marlowe-cli transaction create --testnet-magic "$MAGIC"                  \
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

marlowe-cli contract redeemer --out-file tx-2-first.redeemer
marlowe-cli contract redeemer --out-file tx-2-second.redeemer

# Perform a dry-run of redeeming the first contact: the first party receives
# their 25 ADA and the second party receives their 3 ADA. If the following
# command prints the transaction ID, that indicates that the transaction would
# run without error. (It doesn't matter which party submits this transaction.)

marlowe-cli transaction close --testnet-magic "$MAGIC"                   \
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

marlowe-cli transaction close --testnet-magic "$MAGIC"                   \
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

# Perform a dry-run of redeeming both contracts at once: the first party only
# receives their 25 ADA from the first contract and the second party steals
# the first party's 20 ADA from the second contract because the second validator
# sees that >= 20 ADA is sent to the first party--i.e., this is a double-spend
# attack; the the second party receives 20+3+4=27 ADA put. If the following
# commands succeeds, that indicates that the transaction would run without
# error. (It doesn't matter which party submits this transaction, but the second
# party submits it because they are the thief.)

if cardano-cli transaction build --testnet-magic "$MAGIC" --babbage-era       \
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
then
  echo "Failure! Double satisfaction occurred"
  exit 1
else
  echo "Success. Double satisfaction was prevented"
fi
