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

TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
echo "TIP: $TIP"
# NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
# HOUR="$((3600*1000))"

MINT_EXPIRES=$((TIP + 1000000))
CUR1_NAME=Pear
marlowe-cli util mint "${MAGIC[@]}" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --issuer "$PARTY_ADDR:$PARTY_SKEY" \
  --expires "$MINT_EXPIRES" \
  --out-file /dev/null \
  --submit=600 \
  "$CUR1_NAME:$PARTY_ADDR"
CUR2_NAME=Orange
marlowe-cli util mint "${MAGIC[@]}" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --issuer "$PARTY_ADDR:$PARTY_SKEY" \
  --expires "$MINT_EXPIRES" \
  --out-file /dev/null \
  --submit=600 \
  "$CUR2_NAME:$PARTY_ADDR"
CUR3_NAME=Banana
marlowe-cli util mint "${MAGIC[@]}" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --issuer "$PARTY_ADDR:$PARTY_SKEY" \
  --expires "$MINT_EXPIRES" \
  --out-file /dev/null \
  --submit=600 \
  "$CUR3_NAME:$PARTY_ADDR"

MINIMUM_ADA=3000000

echo '"close"' > close-1.contract
cat close-1.contract

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

CONTRACT_ID=$(
marlowe create \
  --core-file close-1.contract \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign close-1.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "CONTRACT_ID = $CONTRACT_ID"

cardano-cli transaction sign \
  --tx-body-file close-1.txbody \
  --out-file     close-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit close-1.tx

marlowe add "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

CONTRACT_ADDRESS=$(marlowe-cli contract address)
echo "$CONTRACT_ADDRESS"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1,2p;/${CONTRACT_ID//#*/}/p"

marlowe advance --change-address "$PARTY_ADDR" --manual-sign "$PARTY_SKEY" --contract "$CONTRACT_ID"

marlowe rm "$CONTRACT_ID"

marlowe ls

marlowe-cli util burn \
  "${MAGIC[@]}" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --issuer "$PARTY_ADDR:$PARTY_SKEY" \
  --expires $MINT_EXPIRES \
  --token-provider "$PARTY_ADDR:$PARTY_SKEY" \
  --submit 600 \
  --out-file /dev/null

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


