#!/usr/bin/env bash

marlowe-cli --version

cardano-cli --version

git rev-parse HEAD

TREASURY=/extra/iohk/networks/treasury

FAUCET_SKEY=$TREASURY/payment.skey
FAUCET_ADDR=$(cat $TREASURY/payment.testnet.address)
echo "$FAUCET_ADDR"

export CARDANO_NODE_SOCKET_PATH=/tmp/preview.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)
echo "${MAGIC[@]}"

SECOND=1000
MINUTE=$((60 * SECOND))
HOUR=$((60 * MINUTE))
#DAY=$((24 * HOUR))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

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

MINIMUM_ADA=3000000

FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02
INTEREST=$(jq -n $PRINCIPAL*$INTEREST_RATE)

STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")

#LENDING_DEADLINE=$((NOW+12*HOUR))
#REPAYMENT_DEADLINE=$((NOW+24*HOUR))

yaml2json << EOI > history.actus
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
cat history.actus

marlowe-cli template actus \
  --minimum-ada "$MINIMUM_ADA" \
  --party "$PARTY_ADDR" \
  --counter-party "$COUNTERPARTY_ADDR" \
  --actus-terms-file  history.actus \
  --out-contract-file create-1.contract \
  --out-state-file    create-1.state

json2yaml create-1.contract

json2yaml create-1.state

marlowe-cli run initialize \
  --contract-file create-1.contract \
  --state-file    create-1.state    \
  --out-file      create-1.marlowe  \
  --print-stats

marlowe create --help

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

marlowe create \
  --core-file create-1.contract \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign create-1.txbody

cardano-cli transaction sign \
  --tx-body-file create-1.txbody \
  --out-file     create-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit create-1.tx

CONTRACT_ID="$TX_1#1"
echo "$CONTRACT_ID"

marlowe-cli run prepare \
  --marlowe-file create-1.marlowe \
  --out-file     create-2.marlowe \
  --deposit-account "$PARTY_ADDR" \
  --deposit-party "$PARTY_ADDR" \
  --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
  --invalid-before "$((NOW - 5 * MINUTE))" \
  --invalid-hereafter "$((NOW + 1 * HOUR))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file create-2.marlowe \
  --out-file     create-3.marlowe \
  --deposit-account "$COUNTERPARTY_ADDR" \
  --deposit-party "$COUNTERPARTY_ADDR" \
  --deposit-amount "$((INTEREST*FIXED_POINT))" \
  --invalid-before "$((NOW-5*MINUTE))" \
  --invalid-hereafter "$((NOW+1*HOUR))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file create-3.marlowe \
  --out-file     create-4.marlowe \
  --deposit-account "$COUNTERPARTY_ADDR" \
  --deposit-party "$COUNTERPARTY_ADDR" \
  --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
  --invalid-before "$((NOW-5*MINUTE))" \
  --invalid-hereafter "$((NOW+4*HOUR))" \
  --print-stats

jq '.tx.contract' create-4.marlowe | json2yaml

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
