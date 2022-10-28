#!/usr/bin/env bash
echo "Marlowe Runtime Tests: marlowe rm with non empty history"

echo "Test Scneario set up steps:"

echo "Confirm marlowe run executable is installed"
marlowe --help

git rev-parse HEAD

export CARDANO_NODE_SOCKET_PATH=/tmp/node.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

OLD_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2#1
NEW_CONTRACT_ID=02811e36c6cdac4721b53f718c4a1406e09ef0d985f9ad6b7fd676769e2f866c#1
echo "Test Scnario set up done"

echo "Scenario: Removing one contract from list of contracts"
marlowe add $OLD_CONTRACT_ID
marlowe add $NEW_CONTRACT_ID

marlowe rm $OLD_CONTRACT_ID

echo "Expect to see $NEW_CONTRACT_ID only"
marlowe ls

# TODO: Implement --all flag
# marlowe rm --all
marlowe add $OLD_CONTRACT_ID 
marlowe rm $NEW_CONTRACT_ID

# TODO: Implement --all flag
# marlowe rm --all
marlowe add $OLD_CONTRACT_ID 
marlowe rm $OLD_CONTRACT_ID