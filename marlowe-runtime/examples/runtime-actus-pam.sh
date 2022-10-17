#!/usr/bin/env bash

marlowe-cli --version

marlowe --version

cardano-cli --version

git rev-parse HEAD

TREASURY=/extra/iohk/networks/treasury

FAUCET_SKEY=$TREASURY/payment.skey
FAUCET_ADDR=$(cat $TREASURY/payment.testnet.address)
echo "$FAUCET_ADDR"

#source configure.env
echo "${MAGIC[@]}"

SECOND=1000
MINUTE=$((60 * SECOND))
HOUR=$((60 * MINUTE))
#DAY=$((24 * HOUR))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

export CARDANO_NODE_SOCKET_PATH=/tmp/preview.socket
export CARDANO_TESTNET_MAGIC=2
MAGIC=(--testnet-magic 2)

marlowe-cli transaction find-published

PARTY_PREFIX="$TREASURY/john-fletcher"
PARTY_SKEY="$PARTY_PREFIX".skey
PARTY_VKEY="$PARTY_PREFIX".vkey

if [[ ! -e "$PARTY_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_SKEY"      \
                              --verification-key-file "$PARTY_VKEY"
fi
PARTY_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_VKEY")
echo "$PARTY_ADDR"

marlowe-cli util fund-address --out-file /dev/null \
                              --submit 600 \
                              --lovelace 250000000 \
                              --source-wallet-credentials "$FAUCET_ADDR:$FAUCET_SKEY" \
                              "$PARTY_ADDR"

COUNTERPARTY_PREFIX="$TREASURY/thomas-kyd"
COUNTERPARTY_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_VKEY="$COUNTERPARTY_PREFIX".vkey

if [[ ! -e "$COUNTERPARTY_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_SKEY"      \
                              --verification-key-file "$COUNTERPARTY_VKEY"
fi
COUNTERPARTY_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$COUNTERPARTY_VKEY")
echo "$COUNTERPARTY_ADDR"

marlowe-cli util fund-address --out-file /dev/null \
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

yaml2json << EOI > runtime-actus-pam.actus
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
cat runtime-actus-pam.actus

marlowe-cli template actus --minimum-ada "$MINIMUM_ADA" \
                           --party "$PARTY_ADDR" \
                           --counter-party "$COUNTERPARTY_ADDR" \
                           --actus-terms-file runtime-actus-pam.actus \
                           --out-contract-file runtime-actus-pam-1.contract \
                           --out-state-file    runtime-actus-pam-1.state

json2yaml runtime-actus-pam-1.contract

json2yaml runtime-actus-pam-1.state

marlowe-cli run initialize --contract-file runtime-actus-pam-1.contract \
                           --state-file    runtime-actus-pam-1.state    \
                           --out-file      runtime-actus-pam-1.marlowe  \
                           --print-stats

TX_1=$(
marlowe-cli run auto-execute --marlowe-out-file runtime-actus-pam-1.marlowe \
                             --required-signer "$PARTY_SKEY" \
                             --change-address "$PARTY_ADDR" \
                             --out-file runtime-actus-pam-1.raw \
                             --print-stats \
                             --submit 600 \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_1 = $TX_1"

CONTRACT_ID="$TX_1#1"
echo "$CONTRACT_ID"

marlowe-cli run prepare --marlowe-file runtime-actus-pam-1.marlowe \
                        --out-file     runtime-actus-pam-2.marlowe \
                        --deposit-account "$PARTY_ADDR" \
                        --deposit-party "$PARTY_ADDR" \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$((NOW - 5 * MINUTE))" \
                        --invalid-hereafter "$((NOW + 1 * HOUR))" \
                        --print-stats

TX_2=$(
marlowe-cli run auto-execute --tx-in-marlowe "$TX_1#1" \
                             --marlowe-in-file  runtime-actus-pam-1.marlowe \
                             --marlowe-out-file runtime-actus-pam-2.marlowe \
                             --required-signer "$PARTY_SKEY" \
                             --change-address "$PARTY_ADDR" \
                             --out-file runtime-actus-pam-2.raw \
                             --print-stats \
                             --submit 600 \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_2 = $TX_2"

cardano-cli transaction sign --tx-body-file  runtime-actus-pam-2.raw \
  --signing-key-file "$PARTY_SKEY" --out-file runtime-actus-pam-2.signed

marlowe submit runtime-actus-pam-2.signed

marlowe add "$CONTRACT_ID"

marlowe ls --show-status

marlowe rm "$CONTRACT_ID"

marlowe ls

marlowe add "$CONTRACT_ID"

marlowe ls

marlowe ls --all --show-status | wc -l

marlowe ls --all --show-status | head -n 20

marlowe ls --all > all.tmp
marlowe ls --all --show-failed > failed.tmp
diff all.tmp failed.tmp

marlowe log "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

marlowe-cli run prepare --marlowe-file runtime-actus-pam-2.marlowe \
                        --out-file     runtime-actus-pam-3.marlowe \
                        --deposit-account "$COUNTERPARTY_ADDR" \
                        --deposit-party "$COUNTERPARTY_ADDR" \
                        --deposit-amount "$((INTEREST*FIXED_POINT))" \
                        --invalid-before "$((NOW-5*MINUTE))" \
                        --invalid-hereafter "$((NOW+1*HOUR))" \
                        --print-stats

TX_3=$(
marlowe-cli run auto-execute --tx-in-marlowe "$TX_2#1" \
                             --marlowe-in-file  runtime-actus-pam-2.marlowe \
                             --marlowe-out-file runtime-actus-pam-3.marlowe \
                             --required-signer "$COUNTERPARTY_SKEY" \
                             --change-address "$COUNTERPARTY_ADDR" \
                             --out-file runtime-actus-pam-3.raw \
                             --print-stats \
                             --submit 600 \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_3 = $TX_3"

marlowe-cli run prepare --marlowe-file runtime-actus-pam-3.marlowe \
                        --out-file     runtime-actus-pam-4.marlowe \
                        --deposit-account "$COUNTERPARTY_ADDR" \
                        --deposit-party "$COUNTERPARTY_ADDR" \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$((NOW-5*MINUTE))" \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --print-stats

TX_4=$(
marlowe-cli run auto-execute --tx-in-marlowe "$TX_3#1" \
                             --marlowe-in-file  runtime-actus-pam-3.marlowe \
                             --marlowe-out-file runtime-actus-pam-4.marlowe \
                             --required-signer "$COUNTERPARTY_SKEY" \
                             --change-address "$COUNTERPARTY_ADDR" \
                             --out-file runtime-actus-pam-4.raw \
                             --print-stats \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
echo "TX_4 = $TX_4"

cardano-cli transaction sign --tx-body-file  runtime-actus-pam-4.raw \
  --signing-key-file "$COUNTERPARTY_SKEY" --out-file runtime-actus-pam-4.signed

marlowe submit runtime-actus-pam-4.signed

jq '.tx.contract' runtime-actus-pam-4.marlowe | json2yaml

marlowe ls --show-status

marlowe log "$CONTRACT_ID"

marlowe log --show-contract "$CONTRACT_ID"

marlowe --help

marlowe rm "$CONTRACT_ID"

marlowe ls

marlowe create --core-file runtime-actus-pam-1.contract \
               --change-address "$PARTY_ADDR" \
               --min-utxo 3000000 \
               --manual-sign tx.raw

marlowe create --help

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

marlowe-cli util clean --required-signer "$PARTY_SKEY" --change-address "$PARTY_ADDR" --out-file /dev/null --submit 600

echo -n Marlowe | basenc --base16

echo -n 4D61726C6F7765 | tr '[:upper:]' '[:lower:]'

echo -n 4D61726C6F7765 | tr '[:upper:]' '[:lower:]' | basenc --decode --base16

cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDR"

marlowe-cli util select --lovelace-only 1000000 "$PARTY_ADDR"

marlowe-cli util select


