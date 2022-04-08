#!/usr/bin/env bash

set -ev

if [[ -z "${CARDANO_NODE_SOCKET_PATH}" ]]
then
  CARDANO_NODE_SOCKET_PATH=node.socket
fi

# Select network.
MAGIC=(--testnet-magic 1564)

# Wallet and PAB services.
WALLET_API=http://localhost:8090
PAB_API=http://localhost:9080

# The faucet address must correspond to the public key hash of the payment signing key.
FAUCET_KEY="$TREASURY"/payment.skey
FAUCET_ADDRESS=$(cat "$TREASURY"/payment.addr)
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7

# The PAB passphrase must match the `--passphrase` argument of `marlowe-pab`.
PAB_PASSPHRASE=fixme-allow-pass-per-wallet

if [ -z "$1" ]; then
  TEST_CASES="wait refund wallet-failure simple escrow escrow-with-collateral zero-coupon-bond zero-coupon-bond-too-late zero-coupon-bond-immediate-timeout coupon-bond-guaranteed contract-for-differences contract-for-differences-with-oracle swap-of-ada-for-ada"
else
  TEST_CASES=$1
fi


for t in $TEST_CASES
do
  f=test-"$t".yaml
  cabal run marlowe-cli -- test contracts "${MAGIC[@]}"                \
                             --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                             --wallet-url "$WALLET_API"                \
                             --pab-url "$PAB_API"                      \
                             --faucet-key "$FAUCET_KEY"                \
                             --faucet-address "$FAUCET_ADDRESS"        \
                             --burn-address "$BURN_ADDRESS"            \
                             --passphrase "$PAB_PASSPHRASE"            \
                             "$f"                                      \
  | tee "${t%%.yaml}".log
done
