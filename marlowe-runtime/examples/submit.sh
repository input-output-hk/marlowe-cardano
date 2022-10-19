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
#HOUR=$((60 * MINUTE))
#DAY=$((24 * HOUR))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

MINIMUM_ADA=3000000

FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02
INTEREST=$(jq -n $PRINCIPAL*$INTEREST_RATE)

STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")

yaml2json << EOI > submit.actus
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
cat submit.actus

marlowe-cli template actus \
  --minimum-ada "$MINIMUM_ADA" \
  --party "$PARTY_ADDR" \
  --counter-party "$COUNTERPARTY_ADDR" \
  --actus-terms-file  submit.actus \
  --out-contract-file submit-1.contract \
  --out-state-file    submit-1.state

sed -i \
  -e "s/$(jq '.timeout' submit-1.contract)/$((NOW + 10 * MINUTE))/" \
  -e "s/$(($(date -d "$MATURITY_DATE" -u +%s) * SECOND))/$((NOW + 15 * MINUTE))/" \
  submit-1.contract

json2yaml submit-1.contract

json2yaml submit-1.state

marlowe-cli run initialize \
  --contract-file submit-1.contract \
  --state-file    submit-1.state    \
  --out-file      submit-1.marlowe  \
  --print-stats

TX_1=$(
marlowe-cli run auto-execute \
  --marlowe-out-file submit-1.marlowe \
  --required-signer "$PARTY_SKEY" \
  --change-address "$PARTY_ADDR" \
  --out-file submit-1.txbody \
  --print-stats \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_1 = $TX_1"

cardano-cli transaction sign \
  --tx-body-file submit-1.txbody \
  --out-file     submit-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit submit-1.tx

CONTRACT_ID="$TX_1#1"
echo "$CONTRACT_ID"

marlowe-cli run prepare \
  --marlowe-file submit-1.marlowe \
  --out-file     submit-2.marlowe \
  --deposit-account "$PARTY_ADDR" \
  --deposit-party "$PARTY_ADDR" \
  --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
  --invalid-before "$((NOW-5*MINUTE))" \
  --invalid-hereafter "$((NOW+9*MINUTE))" \
  --print-stats

TX_2=$(
marlowe-cli run auto-execute \
  --tx-in-marlowe "$TX_1#1" \
  --marlowe-in-file  submit-1.marlowe \
  --marlowe-out-file submit-2.marlowe \
  --required-signer "$PARTY_SKEY" \
  --change-address "$PARTY_ADDR" \
  --out-file submit-2.txbody \
  --print-stats \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_2 = $TX_2"

cardano-cli transaction sign \
  --tx-body-file submit-2.txbody \
  --out-file     submit-2.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit submit-2.tx

marlowe-cli run prepare \
  --marlowe-file submit-2.marlowe \
  --out-file     submit-3.marlowe \
  --deposit-account "$COUNTERPARTY_ADDR" \
  --deposit-party "$COUNTERPARTY_ADDR" \
  --deposit-amount "$((INTEREST*FIXED_POINT))" \
  --invalid-before "$((NOW-5*MINUTE))" \
  --invalid-hereafter "$((NOW+9*MINUTE))" \
  --print-stats

TX_3=$(
marlowe-cli run auto-execute \
  --tx-in-marlowe "$TX_2#1" \
  --marlowe-in-file  submit-2.marlowe \
  --marlowe-out-file submit-3.marlowe \
  --required-signer "$COUNTERPARTY_SKEY" \
  --change-address "$COUNTERPARTY_ADDR" \
  --out-file submit-3.txbody \
  --print-stats \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_3 = $TX_3"

cardano-cli transaction sign \
  --tx-body-file submit-3.txbody \
  --out-file     submit-3.tx \
  --signing-key-file "$COUNTERPARTY_SKEY"

marlowe submit submit-3.tx

marlowe-cli run prepare \
  --marlowe-file submit-3.marlowe \
  --out-file     submit-4.marlowe \
  --deposit-account "$COUNTERPARTY_ADDR" \
  --deposit-party "$COUNTERPARTY_ADDR" \
  --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
  --invalid-before "$((NOW-5*MINUTE))" \
  --invalid-hereafter "$((NOW+9*MINUTE))" \
  --print-stats

TX_4=$(
marlowe-cli run auto-execute \
  --tx-in-marlowe "$TX_3#1" \
  --marlowe-in-file  submit-3.marlowe \
  --marlowe-out-file submit-4.marlowe \
  --required-signer "$COUNTERPARTY_SKEY" \
  --change-address "$COUNTERPARTY_ADDR" \
  --out-file submit-4.txbody \
  --print-stats \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_4 = $TX_4"

cardano-cli transaction sign \
  --tx-body-file submit-4.txbody \
  --out-file     submit-4.tx \
  --signing-key-file "$COUNTERPARTY_SKEY"

marlowe submit submit-4.tx

jq '.tx.contract' submit-4.marlowe | json2yaml

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
