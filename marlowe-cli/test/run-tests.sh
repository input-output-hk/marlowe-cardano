#!/usr/bin/env bash

set -ev

MAGIC=(--testnet-magic 1564)
CARDANO_NODE_SOCKET_PATH=node.socket
WALLET_API=http://localhost:8090
PAB_API=http://localhost:9080
FAUCET_KEY="$TREASURY"/payment.skey
FAUCET_ADDRESS=addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
PAB_PASSPHRASE=fixme-allow-pass-per-wallet

for t in test-{wait,refund,simple,escrow,escrow-with-collateral,zero-coupon-bond,zero-coupon-bond-immediate-timeout,contract-for-differences,swap-of-ada-for-ada}.yaml
do
  marlowe-cli test contracts "${MAGIC[@]}"                             \
                             --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                             --wallet-url "$WALLET_API"                \
                             --pab-url "$PAB_API"                      \
                             --faucet-key "$FAUCET_KEY"                \
                             --faucet-address "$FAUCET_ADDRESS"        \
                             --burn-address "$BURN_ADDRESS"            \
                             --passphrase "$PAB_PASSPHRASE"            \
                             $t                                        \
  | tee ${t%%.yaml}.log
done
