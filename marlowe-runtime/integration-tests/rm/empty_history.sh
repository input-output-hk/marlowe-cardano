#!/usr/bin/env bash
echo "Marlowe Runtime Tests: marlowe rm with empty history"

echo "Test Scneario set up steps:"

echo "Confirm marlowe run executable is installed"
marlowe --help

git rev-parse HEAD

export CARDANO_NODE_SOCKET_PATH=/tmp/node.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

VALID_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2#1

echo "Test Scnario set up done"

# marlowe rm --all not implemented yet

echo "Scenario: Removing non existent contract"

echo "Expect to see error: 'Contract ID: $VALID_CONTRACT_ID is not managed in history'"
marlowe rm $VALID_CONTRACT_ID

echo "Expect to see no contracts managed in history"
marlowe ls