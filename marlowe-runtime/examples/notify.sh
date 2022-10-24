#!/usr/bin/env bash

marlowe-cli --version

cardano-cli --version

git rev-parse HEAD

TREASURY=treasury

FAUCET_SKEY=$TREASURY/payment.skey
FAUCET_ADDR=$(cat $TREASURY/payment.testnet.address)
echo "$FAUCET_ADDR"

export CARDANO_NODE_SOCKET_PATH=/tmp/preview.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

marlowe-cli transaction find-published

PARTY_SKEY="$TREASURY/john-fletcher.skey"
PARTY_VKEY="$TREASURY/john-fletcher.vkey"

if [[ ! -e "$PARTY_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_SKEY" --verification-key-file "$PARTY_VKEY"
fi
PARTY_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_VKEY")
echo "$PARTY_ADDR"

marlowe-cli util fund-address \
  --out-file /dev/null \
  --submit 600 \
  --lovelace 250000000 \
  --source-wallet-credentials "$FAUCET_ADDR:$FAUCET_SKEY" \
  "$PARTY_ADDR"

SECOND=1000
MINUTE=$((60 * SECOND))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

MINIMUM_ADA=3000000

yaml2json << EOI > notify-1.contract
timeout: $((NOW+15*MINUTE))
timeout_continuation: close
when:
- case:
    notify_if: true
  then: close
EOI
cat notify-1.contract

CONTRACT_ID=$(
marlowe create \
  --core-file notify-1.contract \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$PARTY_ADDR" \
  --manual-sign notify-1.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "CONTRACT_ID = $CONTRACT_ID"

cardano-cli transaction sign \
  --tx-body-file notify-1.txbody \
  --out-file     notify-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit notify-1.tx

marlowe add "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

CONTRACT_ADDR=$(marlowe-cli contract address)
echo "$CONTRACT_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${CONTRACT_ID//#*/}/p"

marlowe notify \
  --contract "$CONTRACT_ID" \
  --validity-lower-bound "$((NOW - 1 * MINUTE))" \
  --validity-upper-bound "$((NOW + 9 * MINUTE))" \
  --change-address "$PARTY_ADDR" \
  --manual-sign notify-2.txbody

cardano-cli transaction sign \
  --tx-body-file notify-2.txbody \
  --out-file     notify-2.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit notify-2.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${CONTRACT_ID//#*/}/p"

marlowe log --show-contract "$CONTRACT_ID"

marlowe rm "$CONTRACT_ID"

marlowe ls

marlowe-cli util clean \
  --change-address "$PARTY_ADDR" \
  --required-signer "$PARTY_SKEY" \
  --out-file /dev/null \
  --submit 600

marlowe-cli transaction simple \
  --tx-in "$(marlowe-cli util select --lovelace-only 1 "$PARTY_ADDR" | sed -n -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;1p')" \
  --required-signer "$PARTY_SKEY" \
  --change-address "$FAUCET_ADDR" \
  --out-file /dev/null \
  --submit 600

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"
