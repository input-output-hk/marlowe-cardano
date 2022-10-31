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

OFFEROR_SKEY="$TREASURY/john-fletcher.skey"
OFFEROR_VKEY="$TREASURY/john-fletcher.vkey"

if [[ ! -e "$OFFEROR_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$OFFEROR_SKEY" --verification-key-file "$OFFEROR_VKEY"
fi
OFFEROR_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$OFFEROR_VKEY")
echo "$OFFEROR_ADDR"

PURCHASER_SKEY="$TREASURY/thomas-kyd.skey"
PURCHASER_VKEY="$TREASURY/thomas-kyd.vkey"

if [[ ! -e "$PURCHASER_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PURCHASER_SKEY" --verification-key-file "$PURCHASER_VKEY"
fi
PURCHASER_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PURCHASER_VKEY")
echo "$PURCHASER_ADDR"

APPROVER_SKEY="$TREASURY/john-webster.skey"
APPROVER_VKEY="$TREASURY/john-webster.vkey"

if [[ ! -e "$APPROVER_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$APPROVER_SKEY" --verification-key-file "$APPROVER_VKEY"
fi
APPROVER_ADDR=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$APPROVER_VKEY")
echo "$APPROVER_ADDR"

marlowe-cli util fund-address \
  --out-file /dev/null \
  --submit 600  \
  --lovelace 125000000 \
  --source-wallet-credentials "$FAUCET_ADDR:$FAUCET_SKEY" \
  "$OFFEROR_ADDR" "$PURCHASER_ADDR" "$APPROVER_ADDR" \
  "$OFFEROR_ADDR" "$PURCHASER_ADDR" "$APPROVER_ADDR"  

TOKEN_NAME=Marlon

POLICY_ID=$(
marlowe-cli util mint \
  --issuer "$FAUCET_ADDR:$FAUCET_SKEY" \
  --count 1000 \
  --out-file /dev/null \
  --submit 600 \
  "$TOKEN_NAME:$OFFEROR_ADDR" \
| sed -e 's/^PolicyID "\(.*\)"$/\1/' \
)
echo "POLICY_ID = $POLICY_ID"

TOKEN="$POLICY_ID.$TOKEN_NAME"
echo "TOKEN = $TOKEN"

SECOND=1000
MINUTE=$((60 * SECOND))

NOW="$(($(date -u +%s) * SECOND))"
echo "$NOW"

ADA=1000000

MINIMUM_ADA=$((3 * ADA))
MAXIMUM_TOKEN=1000
MAXIMUM_ADA=$((200 * ADA))

TOKEN_DEADLINE=$((NOW + 14 * MINUTE))
BID_DEADLINE=$((NOW + 15 * MINUTE))
ACCEPT_DEADLINE=$((NOW + 16 * MINUTE))
ADA_DEADLINE=$((NOW + 17 * MINUTE))
FORBID_DEADLINE=$((NOW + 18 * MINUTE))
NOTIFY_DEADLINE=$((NOW + 30 * MINUTE))

OFFEROR_PARTY="$OFFEROR_ADDR"
PURCHASER_PARTY="Purchaser"
#APPROVER_PARTY="Approver"
OFFEROR_PARTY_C="address: $OFFEROR_ADDR"
PURCHASER_PARTY_C="role_token: Purchaser"
APPROVER_PARTY_C="role_token: Approver"

yaml2json << EOI > token-bid-1.contract
when:
- case:
    choose_between:
    - from: 1
      to: $MAXIMUM_TOKEN
    for_choice:
      choice_name: Offer
      choice_owner:
        $OFFEROR_PARTY_C
  then:
    when:
    - case:
        deposits:
          value_of_choice:
            choice_name: Offer
            choice_owner:
              $OFFEROR_PARTY_C
        into_account:
          $OFFEROR_PARTY_C
        of_token:
          currency_symbol: $POLICY_ID
          token_name: $TOKEN_NAME
        party:
          $OFFEROR_PARTY_C
      then:
        when:
        - case:
            choose_between:
            - from: 1
              to: $MAXIMUM_ADA
            for_choice:
              choice_name: Bid
              choice_owner:
                $PURCHASER_PARTY_C
          then:
            when:
            - case:
                choose_between:
                - from: 1
                  to: 1
                for_choice:
                  choice_name: Accept
                  choice_owner:
                    $OFFEROR_PARTY_C
              then:
                when:
                - case:
                    deposits:
                      value_of_choice:
                        choice_name: Bid
                        choice_owner:
                          $PURCHASER_PARTY_C
                    into_account:
                      $PURCHASER_PARTY_C
                    of_token:
                      currency_symbol: ''
                      token_name: ''
                    party:
                      $PURCHASER_PARTY_C
                  then:
                    when:
                    - case:
                        choose_between:
                        - from: 1
                          to: 1
                        for_choice:
                          choice_name: Forbid
                          choice_owner:
                            $APPROVER_PARTY_C
                      then: close
                    timeout: $FORBID_DEADLINE
                    timeout_continuation:
                      pay:
                        amount_of_token:
                          currency_symbol: ''
                          token_name: ''
                        in_account:
                          $PURCHASER_PARTY_C
                      from_account:
                        $PURCHASER_PARTY_C
                      token:
                        currency_symbol: ''
                        token_name: ''
                      to:
                        account:
                          $OFFEROR_PARTY_C
                      then:
                        pay:
                          amount_of_token:
                            currency_symbol: $POLICY_ID
                            token_name: $TOKEN_NAME
                          in_account:
                            $OFFEROR_PARTY_C
                        from_account:
                          $OFFEROR_PARTY_C
                        token:
                          currency_symbol: $POLICY_ID
                          token_name: $TOKEN_NAME
                        to:
                          party:
                            $PURCHASER_PARTY_C
                        then:
                          when:
                          - case:
                              notify_if: true
                            then: close
                          timeout: $NOTIFY_DEADLINE
                          timeout_continuation: close
                timeout: $ADA_DEADLINE
                timeout_continuation: close
            timeout: $ACCEPT_DEADLINE
            timeout_continuation: close
        timeout: $BID_DEADLINE
        timeout_continuation: close
    timeout: $TOKEN_DEADLINE
    timeout_continuation: close
timeout: $TOKEN_DEADLINE
timeout_continuation: close
EOI
cat token-bid-1.contract

yaml2json << EOI > token-bid-1.state
accounts:
- - - address: $APPROVER_ADDR
    - currency_symbol: ''
      token_name: ''
  - $MINIMUM_ADA
boundValues: []
choices: []
minTime: 1
EOI
cat token-bid-1.state

marlowe-cli run initialize \
  --contract-file token-bid-1.contract \
  --state-file    token-bid-1.state \
  --out-file      token-bid-1.marlowe \
  --roles-currency "00000000000000000000000000000000000000000000000000000000" \
  --permanently-without-staking \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-1.marlowe \
  --out-file     token-bid-2.marlowe \
  --choice-party "$OFFEROR_PARTY" \
  --choice-name Offer \
  --choice-number "$((MAXIMUM_TOKEN / 2))" \
  --invalid-before    "$((NOW + 1 * MINUTE))" \
  --invalid-hereafter "$((NOW + 2 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-2.marlowe \
  --out-file     token-bid-3.marlowe \
  --deposit-account "$OFFEROR_PARTY" \
  --deposit-party "$OFFEROR_PARTY" \
  --deposit-amount "$((MAXIMUM_TOKEN / 2))" \
  --deposit-token "$TOKEN" \
  --invalid-before    "$((NOW + 1 * MINUTE))" \
  --invalid-hereafter "$((NOW + 2 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-3.marlowe \
  --out-file     token-bid-4.marlowe \
  --choice-party "$PURCHASER_PARTY" \
  --choice-name Bid \
  --choice-number "$((MAXIMUM_ADA / 2))" \
  --invalid-before    "$((NOW + 1 * MINUTE))" \
  --invalid-hereafter "$((NOW + 2 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-4.marlowe \
  --out-file     token-bid-5.marlowe \
  --choice-party "$OFFEROR_PARTY" \
  --choice-name Accept \
  --choice-number 1 \
  --invalid-before    "$((NOW + 1 * MINUTE))" \
  --invalid-hereafter "$((NOW + 2 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-5.marlowe \
  --out-file     token-bid-6.marlowe \
  --deposit-account "$PURCHASER_PARTY" \
  --deposit-party "$PURCHASER_PARTY" \
  --deposit-amount "$((MAXIMUM_ADA / 2))" \
  --invalid-before    "$((NOW + 1 * MINUTE))" \
  --invalid-hereafter "$((NOW + 2 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-6.marlowe \
  --out-file     token-bid-7.marlowe \
  --invalid-before    "$((NOW + 20 * MINUTE))" \
  --invalid-hereafter "$((NOW + 21 * MINUTE))" \
  --print-stats

marlowe-cli run prepare \
  --marlowe-file token-bid-7.marlowe \
  --out-file     token-bid-8.marlowe \
  --notify \
  --invalid-before    "$((NOW + 22 * MINUTE))" \
  --invalid-hereafter "$((NOW + 23 * MINUTE))" \
  --print-stats

cardano-cli query utxo "${MAGIC[@]}" --address "$OFFEROR_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$PURCHASER_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$APPROVER_ADDR"

TX_1=$(
marlowe create \
  --core-file token-bid-1.contract \
  --role="Purchaser=$PURCHASER_ADDR" \
  --role="Approver=$APPROVER_ADDR" \
  --min-utxo "$MINIMUM_ADA" \
  --change-address "$APPROVER_ADDR" \
  --manual-sign token-bid-1.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
CONTRACT_ID="$TX_1"
echo "CONTRACT_ID = TX_1 = $CONTRACT_ID"

cardano-cli transaction sign \
  --tx-body-file token-bid-1.txbody \
  --out-file     token-bid-1.tx \
  --signing-key-file "$APPROVER_SKEY"

marlowe submit token-bid-1.tx

TX_2=$(
marlowe choose \
  --contract "$CONTRACT_ID" \
  --party "$OFFEROR_PARTY" \
  --choice Offer \
  --value "$((MAXIMUM_TOKEN / 2))" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$APPROVER_ADDR" \
  --address "$OFFEROR_ADDR" \
  --manual-sign token-bid-2.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_2 = $TX_2"

cardano-cli transaction sign \
  --tx-body-file token-bid-2.txbody \
  --out-file     token-bid-2.tx \
  --signing-key-file "$OFFEROR_SKEY" \
  --signing-key-file "$APPROVER_SKEY"

marlowe submit token-bid-2.tx

TX_3=$(
marlowe deposit \
  --contract "$CONTRACT_ID" \
  --to-party "$OFFEROR_PARTY" \
  --from-party "$OFFEROR_PARTY" \
  --quantity "$((MAXIMUM_TOKEN / 2))" \
  --currency "$POLICY_ID" \
  --token-name "$TOKEN_NAME" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --address "$OFFEROR_ADDR" \
  --address "$APPROVER_ADDR" \
  --change-address "$OFFEROR_ADDR" \
  --manual-sign token-bid-3.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_3 = $TX_3"

cardano-cli transaction sign \
  --tx-body-file token-bid-3.txbody \
  --out-file     token-bid-3.tx \
  --signing-key-file "$OFFEROR_SKEY" \
  --signing-key-file "$APPROVER_SKEY"

marlowe submit token-bid-3.tx

TX_4=$(
marlowe choose \
  --contract "$CONTRACT_ID" \
  --party "$PURCHASER_PARTY" \
  --choice Bid \
  --value "$((MAXIMUM_ADA / 2))" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$PURCHASER_ADDR" \
  --manual-sign token-bid-4.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_4 = $TX_4"

cardano-cli transaction sign \
  --tx-body-file token-bid-4.txbody \
  --out-file     token-bid-4.tx \
  --signing-key-file "$PURCHASER_SKEY"

marlowe submit token-bid-4.tx

TX_5=$(
marlowe choose \
  --contract "$CONTRACT_ID" \
  --party "$OFFEROR_PARTY" \
  --choice Accept \
  --value 1 \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$PURCHASER_ADDR" \
  --manual-sign token-bid-5.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_5 = $TX_5"

cardano-cli transaction sign \
  --tx-body-file token-bid-5.txbody \
  --out-file     token-bid-5.tx \
  --signing-key-file "$OFFEROR_SKEY" \
  --signing-key-file "$PURCHASER_SKEY"

marlowe submit token-bid-5.tx

TX_6=$(
marlowe deposit \
  --contract "$CONTRACT_ID" \
  --to-party "$PURCHASER_PARTY" \
  --from-party "$PURCHASER_PARTY" \
  --lovelace "$((MAXIMUM_ADA / 2))" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$PURCHASER_ADDR" \
  --manual-sign token-bid-6.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_6 = $TX_6"

cardano-cli transaction sign \
  --tx-body-file token-bid-6.txbody \
  --out-file     token-bid-6.tx \
  --signing-key-file "$PURCHASER_SKEY"

marlowe submit token-bid-6.tx

WAIT="$(((FORBID_DEADLINE - $(date -u +%s) * SECOND + 2 * MINUTE + 1 * SECOND) / SECOND))"
if [[ $WAIT -ge 0 ]]
then
  echo "Waiting $WAIT seconds before the timeout passes."
  sleep "$WAIT"
fi

TX_7=$(
marlowe advance \
  --contract "$CONTRACT_ID" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 2 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$APPROVER_ADDR" \
  --address "$OFFEROR_ADDR" \
  --manual-sign token-bid-7.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_7 = $TX_7"

cardano-cli transaction sign \
  --tx-body-file token-bid-7.txbody \
  --out-file     token-bid-7.tx \
  --signing-key-file "$OFFEROR_SKEY" \
  --signing-key-file "$APPROVER_SKEY"

marlowe submit token-bid-7.tx

TX_8=$(
marlowe notify \
  --contract "$CONTRACT_ID" \
  --validity-lower-bound "$(($(date -u +%s) * SECOND - 1 * MINUTE))" \
  --validity-upper-bound "$(($(date -u +%s) * SECOND + 2 * MINUTE))" \
  --change-address "$OFFEROR_ADDR" \
  --manual-sign token-bid-8.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_8 = $TX_8"

cardano-cli transaction sign \
  --tx-body-file token-bid-8.txbody \
  --out-file     token-bid-8.tx \
  --signing-key-file "$OFFEROR_SKEY"

marlowe submit token-bid-8.tx

TX_9=$(
marlowe withdraw \
  --contract "$CONTRACT_ID" \
  --role "$PURCHASER_PARTY" \
  --change-address "$PURCHASER_ADDR" \
  --manual-sign token-bid-9.txbody \
| sed -e 's/^.*"\([^\\]*\)\\.*$/\1/' \
)
echo "TX_9 = $TX_9"

cardano-cli transaction sign \
  --tx-body-file token-bid-9.txbody \
  --out-file     token-bid-9.tx \
  --signing-key-file "$PURCHASER_SKEY"

marlowe submit token-bid-9.tx

cardano-cli query utxo "${MAGIC[@]}" --address "$OFFEROR_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$PURCHASER_ADDR"

cardano-cli query utxo "${MAGIC[@]}" --address "$APPROVER_ADDR"
