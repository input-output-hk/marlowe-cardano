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

FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02
INTEREST=$(jq -n $PRINCIPAL*$INTEREST_RATE)

STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")

yaml2json << EOI > deposit.actus
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
cat deposit.actus

marlowe-cli template actus \
  --minimum-ada "$MINIMUM_ADA" \
  --party "$PARTY_ADDR" \
  --counter-party "$COUNTERPARTY_ADDR" \
  --actus-terms-file  deposit.actus \
  --out-contract-file deposit-1.contract \
  --out-state-file /dev/null

sed -i \
  -e "s/$(jq '.timeout' deposit-1.contract)/$((NOW + 10 * MINUTE))/" \
  -e "s/$(($(date -d "$MATURITY_DATE" -u +%s) * SECOND))/$((NOW + 15 * MINUTE))/" \
  deposit-1.contract

json2yaml deposit-1.contract

TX_1=$(
marlowe create \
  --core-file deposit-1.contract \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign deposit-1.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
CONTRACT_ID="$TX_1"
echo "CONTRACT_ID = TX_1 = $CONTRACT_ID"

cardano-cli transaction sign \
  --tx-body-file deposit-1.txbody \
  --out-file     deposit-1.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit deposit-1.tx

CONTRACT_ADDR=$(marlowe-cli contract address)
echo "$CONTRACT_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${TX_1//#*/}/p"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDR"

marlowe add "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

TX_2=$(
marlowe deposit \
  --contract "$CONTRACT_ID" \
  --from-party "$PARTY_ADDR" \
  --to-party "$PARTY_ADDR" \
  --lovelace "$((PRINCIPAL*FIXED_POINT))" \
  --validity-lower-bound "$((NOW-5*MINUTE))" \
  --validity-upper-bound "$((NOW+9*MINUTE))" \
  --change-address "$PARTY_ADDR" \
  --address "$PARTY_ADDR" \
  --manual-sign deposit-2.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_2 = $TX_2"

cardano-cli transaction sign \
  --tx-body-file deposit-2.txbody \
  --out-file     deposit-2.tx \
  --signing-key-file "$PARTY_SKEY"

marlowe submit deposit-2.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${TX_2//#*/}/p"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDR"

marlowe log --show-contract "$CONTRACT_ID"

TX_3=$(
marlowe deposit \
  --contract "$CONTRACT_ID" \
  --from-party "$COUNTERPARTY_ADDR" \
  --to-party "$COUNTERPARTY_ADDR" \
  --lovelace "$((INTEREST*FIXED_POINT))" \
  --validity-lower-bound "$((NOW-5*MINUTE))" \
  --validity-upper-bound "$((NOW+9*MINUTE))" \
  --change-address "$COUNTERPARTY_ADDR" \
  --address "$COUNTERPARTY_ADDR" \
  --manual-sign deposit-3.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_3 = $TX_3"

cardano-cli transaction sign \
  --tx-body-file deposit-3.txbody \
  --out-file     deposit-3.tx \
  --signing-key-file "$COUNTERPARTY_SKEY"

marlowe submit deposit-3.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${TX_3//#*/}/p"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDR"

marlowe log --show-contract "$CONTRACT_ID"

TX_4=$(
marlowe deposit \
  --contract "$CONTRACT_ID" \
  --from-party "$COUNTERPARTY_ADDR" \
  --to-party "$COUNTERPARTY_ADDR" \
  --lovelace "$((PRINCIPAL*FIXED_POINT))" \
  --validity-lower-bound "$((NOW-5*MINUTE))" \
  --validity-upper-bound "$((NOW+9*MINUTE))" \
  --change-address "$COUNTERPARTY_ADDR" \
  --address "$COUNTERPARTY_ADDR" \
  --manual-sign deposit-4.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_4 = $TX_4"

cardano-cli transaction sign \
  --tx-body-file deposit-4.txbody \
  --out-file     deposit-4.tx \
  --signing-key-file "$COUNTERPARTY_SKEY"

marlowe submit deposit-4.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDR" | sed -n -e "1,2p;/${TX_4//#*/}/p"

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDR"

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
