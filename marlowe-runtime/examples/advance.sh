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

COUNTERPARTY_SKEY="$TREASURY/thomas-kyd.skey"
COUNTERPARTY_VKEY="$TREASURY/thomas-kyd.vkey"

if [[ ! -e "$COUNTERPARTY_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_SKEY" --verification-key-file "$COUNTERPARTY_VKEY"
fi
COUNTERPARTY_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$COUNTERPARTY_VKEY")
echo "$COUNTERPARTY_ADDR"

marlowe-cli util fund-address \
  --out-file /dev/null \
  --submit 600  \
  --lovelace 250000000 \
  --source-wallet-credentials "$FAUCET_ADDR:$FAUCET_SKEY" \
  "$COUNTERPARTY_ADDR"

SECOND=1000
MINUTE=$((60 * SECOND))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

MINIMUM_ADA=3000000

#FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02

STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")

yaml2json << EOI > advance.actus
scheduleConfig:
  businessDayConvention: "NULL"
  endOfMonthConvention: "EOM"
  calendar: "NC"
maturityDate: "$MATURITY_DATE"
contractId: "0"
enableSettlement: false
initialExchangeDate: "$INITIAL_EXCHANGE_DATE"
contractRole: "RPA"
penaltyType: "O"
cycleAnchorDateOfInterestPayment: "$INITIAL_EXCHANGE_DATE"
contractType: "PAM"
notionalPrincipal: $PRINCIPAL
contractPerformance: "PF"
collateralAmount: 0
dayCountConvention: "30E360"
accruedInterest: 0
statusDate: "$STATUS_DATE"
cycleOfInterestPayment: "P1YL1"
prepaymentEffect: "N"
nominalInterestRate: $INTEREST_RATE
interestCalculationBase: "NT"
EOI
cat advance.actus

marlowe-cli template actus \
  --minimum-ada "$MINIMUM_ADA" \
  --party "$PARTY_ADDR" \
  --counter-party "$COUNTERPARTY_ADDR" \
  --actus-terms-file  advance.actus \
  --out-contract-file advance-1.contract \
  --out-state-file /dev/null

sed -i \
  -e "s/$(jq '.timeout' advance-1.contract)/$((NOW + 10 * MINUTE))/" \
  -e "s/$(($(date -d "$MATURITY_DATE" -u +%s) * SECOND))/$((NOW + 15 * MINUTE))/" \
  advance-1.contract

json2yaml advance-1.contract

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

CONTRACT_ID=$(
marlowe create \
  --core-file advance-1.contract \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign advance-1.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "CONTRACT_ID = $CONTRACT_ID"

cardano-cli transaction sign \
  --tx-body-file advance-1.txbody \
  --out-file     advance-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit advance-1.tx

marlowe add "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

CONTRACT_ADDRESS=$(marlowe-cli contract address)
echo "$CONTRACT_ADDRESS"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1,2p;/${CONTRACT_ID//#*/}/p"

sleep 11m

marlowe advance \
  --contract "$CONTRACT_ID" \
  --validity-lower-bound "$(($(jq '.timeout' advance-1.contract) + 1 * SECOND))" \
  --validity-upper-bound "$(($(jq '.timeout' advance-1.contract) + 4 * MINUTE))" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign advance-2.txbody

cardano-cli transaction sign \
  --tx-body-file advance-2.txbody \
  --out-file     advance-2.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit advance-2.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1,2p;/${CONTRACT_ID//#*/}/p"

marlowe log --show-contract "$CONTRACT_ID"

marlowe rm "$CONTRACT_ID"

marlowe ls

marlowe-cli util clean \
  --change-address "$PARTY_ADDR" \
  --required-signer "$PARTY_SKEY" \
  --out-file /dev/null \
  --submit 600

marlowe-cli util clean \
  --change-address "$COUNTERPARTY_ADDR" \
  --required-signer "$COUNTERPARTY_SKEY" \
  --out-file /dev/null \
  --submit 600

marlowe-cli transaction simple \
  --tx-in "$(marlowe-cli util select --lovelace-only 1 "$PARTY_ADDR" | sed -n -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;1p')" \
  --tx-in "$(marlowe-cli util select --lovelace-only 1 "$COUNTERPARTY_ADDR" | sed -n -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;1p')" \
  --required-signer "$PARTY_SKEY" \
  --required-signer "$COUNTERPARTY_SKEY" \
  --change-address "$FAUCET_ADDR" \
  --out-file /dev/null \
  --submit 600

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR" --address "$COUNTERPARTY_ADDR"
