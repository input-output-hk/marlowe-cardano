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
# FAUCET_VKEY - public key for the faucet.

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/burn.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/burn-distributed.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/burn-multi-asset.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/mint.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/mint-distributed.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/return-funds.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/operations/runtime.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/cli/trivial.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/cli/escrow.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/cli/covered-call.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/cli/zero-coupon-bond.yaml

cabal run marlowe-cli -- \
 --babbage-era test \
 --testnet-magic 2 \
 --socket-path "$CARDANO_NODE_SOCKET_PATH" \
 --faucet-key "$FAUCET_SKEY_FILE" \
 --faucet-address "$FAUCET_VKEY" \
 --marlowe-runtime-port "$MARLOWE_RT_PORT" \
 --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
 --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
 ./test/templates/runtime/trivial.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/runtime/swap.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/runtime/escrow.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/runtime/zero-coupon-bond.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/runtime/covered-call.yaml

cabal run marlowe-cli -- \
  --babbage-era test \
  --testnet-magic 2 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-key "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_VKEY" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
  --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
  ./test/templates/cli/role-based/escrow.yaml

# WIP: PLT-5528
# cabal run marlowe-cli -- \
#   --babbage-era test \
#   --testnet-magic 2 \
#   --socket-path "$CARDANO_NODE_SOCKET_PATH" \
#   --faucet-key "$FAUCET_SKEY_FILE" \
#   --faucet-address "$FAUCET_VKEY" \
#   --marlowe-runtime-port "$MARLOWE_RT_PORT" \
#   --chain-seek-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
#   --chain-seek-cmd-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
#   ./test/templates/runtime/role-based/escrow.yaml
