#!/usr/bin/env bash

echo "Marlowe Runtime Tests: marlowe add with empty history"

echo "Test Scneario set up steps:"

echo "Confirm marlowe run executable is installed"
marlowe --help

echo "set env variables"
export CARDANO_NODE_SOCKET_PATH=/tmp/preview.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

VALID_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2#1
INVALID_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2
MISSING_CONTRACT_ID=
EMPTY_STRING_CONTRACT_ID=""
marlowe rm --all

echo "Test Scnario set up done"

marlowe ls


echo "Scenario: Adding a valid contract id to an empty history"

marlowe add $VALID_CONTRACT_ID

echo "Expect to see only $VALID_CONTRACT_ID managed in history"
marlowe ls

marlowe rm $VALID_CONTRACT_ID

marlowe ls

echo "Scenario: Adding an invalid contract id to an empty history"

echo "Expect to see an error: Invalid UTXO - expected format: <hex-tx-id>#<tx-out-ix>"
marlowe add $INVALID_CONTRACT_ID

echo "Expect to see an error: Invalid UTXO - expected format: <hex-tx-id>#<tx-out-ix>"
marlowe add $MISSING_CONTRACT_ID

echo "Expect to see an error: Invalid UTXO - expected format: <hex-tx-id>#<tx-out-ix>"
marlowe add $EMPTY_STRING_CONTRACT_ID
