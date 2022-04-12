#!/usr/bin/env bash

if [[ -z "$CARDANO_NODE_SOCKET_PATH" ]]
then
  CARDANO_NODE_SOCKET_PATH=node.socket
fi

# Select network.
MAGIC=(--testnet-magic 1564)

# Wallet and PAB services.
WALLET_API=http://localhost:8090
PAB_API=http://localhost:9080

# The PAB passphrase must match the `--passphrase` argument of `marlowe-pab`.
PAB_PASSPHRASE=fixme-allow-pass-per-wallet

# The burn address is arbitrary.
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7

# Keys to the faucet for PAB testing.
FAUCET_SKEY="$TREASURY"/payment.skey
FAUCET_VKEY="$TREASURY"/payment.vkey

# Create the payment signing and verification keys if they do not already exist.
if [[ ! -e "$FAUCET_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$FAUCET_SKEY"      \
                              --verification-key-file "$FAUCET_VKEY"
fi
FAUCET_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$FAUCET_VKEY")

# Fund the faucet with 25k tADA.
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 25000000000                    \
                        "$FAUCET_ADDRESS"                         \
> /dev/null

# Record the git hash and run the tests.

git log -n 1 --pretty=oneline

echo "Non-PAB tests:"

if bash -ve "test/double-satisfaction.sh" >& "test/double-satisfaction.log"
then
    echo "  PASS: test/double-satisfaction"
else
    echo "  FAIL: test/double-satisfaction"
fi

for t in examples/*/run-*.sh
do
  if bash -ve "$t" >& "${t%%.sh}.log"
  then
    echo "  PASS: $t"
  else
    echo "  FAIL: $t"
  fi
done

echo  "PAB tests:"

for t in test/test-{wait,refund,wallet-failure,simple,escrow,escrow-with-collateral,zero-coupon-bond,zero-coupon-bond-too-late,zero-coupon-bond-immediate-timeout,coupon-bond-guaranteed,contract-for-differences,contract-for-differences-with-oracle,swap-of-ada-for-ada}.yaml
do
  if marlowe-cli test contracts "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --wallet-url "$WALLET_API"                \
                                --pab-url "$PAB_API"                      \
                                --faucet-key "$FAUCET_SKEY"               \
                                --faucet-address "$FAUCET_ADDRESS"        \
                                --burn-address "$BURN_ADDRESS"            \
                                --passphrase "$PAB_PASSPHRASE"            \
                                $t                                        \
                                >& ${t%%.yaml}.log
  then
    echo "  PASS: $t"
  else
    echo "  FAIL: $t"
  fi
done
