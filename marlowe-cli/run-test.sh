#!/usr/bin/env bash
#
# Quick and dirty way to run the tests:
#
# Most of the variables can be set using source ../scripts/local-docker.env but the CARDANO_NODE_SOCKET_PATH is broken there and you should reset it by:
#
# export CARDANO_NODE_SOCKET_PATH="$CARDANO_NODE_SOCKET_PATH"/node.socket
#
# Required variables:
#
# CARDANO_NODE_SOCKET_PATH - path to the cardno-node socket - requires read / write permissions.
# MARLOWE_RT_PORT - port to run the marlowe runtime server.
# MARLOWE_CHAIN_SYNC_PORT - port to run the chain sync server.
# MARLOWE_CHAIN_SYNC_COMMAND_PORT - port to run the chain sync command server.
#
# FAUCET_SKEY_FILE - private key file for the faucet.
# FAUCET_VKEY - faucet address.
#
cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --concurrent-runners 6 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-skey-file "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  --write-to-json-file report.json \
  --max-retries 3 \
  --stream-json \
  ./test/operations/burn-distributed.yaml \
  ./test/operations/burn-multi-asset.yaml \
  ./test/operations/burn.yaml \
  ./test/operations/mint-distributed.yaml \
  ./test/operations/mint.yaml \
  ./test/operations/return-funds.yaml \
  ./test/operations/runtime.yaml \
  ./test/templates/cli/covered-call.yaml \
  ./test/templates/cli/escrow.yaml \
  ./test/templates/cli/swap.yaml \
  ./test/templates/cli/trivial.yaml \
  ./test/templates/cli/zero-coupon-bond.yaml \
  ./test/templates/cli/role-based/covered-call.yaml \
  ./test/templates/cli/role-based/escrow.yaml \
  ./test/templates/cli/role-based/swap.yaml \
  ./test/templates/cli/role-based/zero-coupon-bond.yaml \
  ./test/templates/runtime/covered-call.yaml \
  ./test/templates/runtime/escrow.yaml \
  ./test/templates/runtime/swap.yaml \
  ./test/templates/runtime/trivial.yaml \
  ./test/templates/runtime/zero-coupon-bond.yaml \
  ./test/templates/runtime/role-based/covered-call.yaml \
  ./test/templates/runtime/role-based/escrow.yaml \
  ./test/templates/runtime/role-based/swap.yaml \
  ./test/templates/runtime/role-based/zero-coupon-bond.yaml \


