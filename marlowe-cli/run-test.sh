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
# FAUCET_ADDRESS - faucet address.
#
#
cabal run exe:marlowe-cli -- \
  --conway-era test \
  --testnet-magic "$CARDANO_NODE_NETWORK_ID" \
  --max-concurrent-runners 8 \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --faucet-skey-file "$FAUCET_SKEY_FILE" \
  --faucet-address "$FAUCET_ADDRESS" \
  --marlowe-runtime-port "$MARLOWE_RT_PORT" \
  --write-to-json-file report.json \
  --max-retries 3 \
  --stream-json \
  ./test/operations/runtime.yaml 
  ./test/inline/role-based/cli/contract-for-differences-with-oracle.yaml \
  ./test/inline/role-based/cli/contract-for-differences.yaml \
  ./test/inline/role-based/cli/coupon-bond-guaranteed.yaml \
  ./test/inline/role-based/cli/escrow-with-collateral.yaml \
  ./test/inline/role-based/cli/escrow.yaml \
  ./test/inline/role-based/cli/open-escrow-with-collateral.yaml \
  ./test/inline/role-based/cli/open-swap-for-ada.yaml \
  ./test/inline/role-based/cli/swap-of-ada-for-ada.yaml \
  ./test/inline/role-based/cli/zero-coupon-bond-immediate-timeout.yaml \
  ./test/inline/role-based/cli/zero-coupon-bond-too-late.yaml \
  ./test/inline/role-based/cli/zero-coupon-bond.yaml \
  ./test/on-chain-limits/accounts-map-size.yaml \
  ./test/on-chain-limits/deposits-chain-with-open-roles-and-merkleization.yaml \
  ./test/on-chain-limits/deposits-chain-with-open-roles.yaml \
  ./test/on-chain-limits/deposits-chain-with-roles.yaml \
  ./test/on-chain-limits/deposits-chain.yaml \
  ./test/on-chain-limits/payouts-chain.yaml \
  ./test/operations/burn-distributed.yaml \
  ./test/operations/burn-multi-asset.yaml \
  ./test/operations/burn.yaml \
  ./test/operations/mint-distributed.yaml \
  ./test/operations/mint-multiple.yaml \
  ./test/operations/mint-to-open-role-script.yaml \
  ./test/operations/mint.yaml \
  ./test/operations/open-role-based-choice.yaml \
  ./test/operations/open-role-based-choice.yaml \
  ./test/operations/open-role-based-deposit-mixed-with-other-inputs.yaml \
  ./test/operations/open-role-based-deposit.yaml \
  ./test/operations/open-role-multi-token-release.yaml \
  ./test/operations/open-role-release-fails-without-thread-token.yaml \
  ./test/operations/open-role-release-fails-without-thread-token.yaml \
  ./test/operations/open-role-uses-thread-token-name-to-release-the-token.yaml \
  ./test/operations/publish.yaml \
  ./test/operations/return-funds.yaml \
  ./test/operations/runtime-with-client-side-merkleization.yaml \
  ./test/operations/runtime.yaml \
  ./test/templates/address-based/cli/covered-call.yaml \
  ./test/templates/address-based/cli/escrow.yaml \
  ./test/templates/address-based/cli/raffle.yaml \
  ./test/templates/address-based/cli/swap.yaml \
  ./test/templates/address-based/cli/trivial.yaml \
  ./test/templates/address-based/cli/zero-coupon-bond.yaml \
  ./test/templates/address-based/runtime/covered-call.yaml \
  ./test/templates/address-based/runtime/escrow.yaml \
  ./test/templates/address-based/runtime/swap.yaml \
  ./test/templates/address-based/runtime/trivial.yaml \
  ./test/templates/address-based/runtime/zero-coupon-bond.yaml \
  ./test/templates/role-based/cli/covered-call.yaml \
  ./test/templates/role-based/cli/escrow.yaml \
  ./test/templates/role-based/cli/swap.yaml \
  ./test/templates/role-based/cli/zero-coupon-bond.yaml \
  ./test/templates/role-based/runtime/chunked-value-transfer.yaml \
  ./test/templates/role-based/runtime/covered-call.yaml \
  ./test/templates/role-based/runtime/escrow.yaml \
  ./test/templates/role-based/runtime/swap.yaml \
  ./test/templates/role-based/runtime/zero-coupon-bond.yaml \

# Fails in simulation mode - PLT-7509:
# ./test/inline/role-based/cli/zero-coupon-bond-delayed-timeout.yaml \
