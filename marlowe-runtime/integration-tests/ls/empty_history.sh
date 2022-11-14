#!/usr/bin/env bash

echo "Marlowe Runtime Tests: marlowe list with empty history"

echo "Test Scneario set up steps:"

echo "Confirm marlowe run executable is installed"
marlowe --help

echo "set env variables"
export CARDANO_NODE_SOCKET_PATH=/tmp/preview.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

marlowe rm --all

echo "Test Scnario set up done"

marlowe ls

echo "Expect to see a message saying: No contracts are listed in history"