#!/usr/bin/env bash

set -evo pipefail


. /extra/iohk/networks/preprod/configure.env

export PATH=$MARLOWE_BIN:$PATH


SKEY=demo.skey
ADDR=$(cat demo.testnet.address)
echo $ADDR

POLICY_ID=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d

ADA=1000000


ROLES=(c.marlowe e.cary f.beaumont j.lumley j.webster m.herbert w.shakespeare)


function randomWait {
  sleep $((10 + RANDOM % 5))s
}

function randomRole {
  echo ${ROLES[$((RANDOM % ${#ROLES[@]}))]}
}

function randomChoice {
  echo $((RANDOM % 101))
}

function marloweSubmit {
LAST_TX=$(
marlowe-cli run auto-execute \
  --tx-in-marlowe $LAST_TX \
  --marlowe-in-file marlowe-$(($1-1)).json \
  --marlowe-out-file marlowe-$1.json \
  --change-address $ADDR \
  --required-signer $SKEY \
  --metadata-file metadata.json \
  --out-file /dev/null \
  --submit 600s \
| sed -e 's/^TxId "\(.*\)"$/\1#1/' \
)
echo $LAST_TX
randomWait
}

function marloweDeposit {
  marlowe-cli run prepare \
    --deposit-party $2 \
    --deposit-account $2 \
    --deposit-amount $3 \
    --invalid-before $((1000 * ($(date -u +%s) - 1 * 60))) \
    --invalid-hereafter $((1000 * ($(date -u +%s) + 5 * 60))) \
    --marlowe-file marlowe-$(($1-1)).json \
    --out-file marlowe-$1.json
  marloweSubmit $1
}

function marloweChoose {
  marlowe-cli run prepare \
    --choice-party $2 \
    --choice-name $3 \
    --choice-number $4 \
    --invalid-before $((1000 * ($(date -u +%s) - 1 * 60))) \
    --invalid-hereafter $((1000 * ($(date -u +%s) + 5 * 60))) \
    --marlowe-file marlowe-$(($1-1)).json \
    --out-file marlowe-$1.json
  marloweSubmit $1
}

function marloweNotify {
  marlowe-cli run prepare \
    --notify \
    --invalid-before $((1000 * ($(date -u +%s) - 1 * 60))) \
    --invalid-hereafter $((1000 * ($(date -u +%s) + 5 * 60))) \
    --marlowe-file marlowe-$(($1-1)).json \
    --out-file marlowe-$1.json
  marloweSubmit $1
}


cardano-cli query utxo ${MAGIC[@]} --address $ADDR

marlowe-cli util clean \
  --lovelace $((2 * ADA)) \
  --change-address $ADDR \
  --required-signer $SKEY \
  --out-file /dev/null \
  --submit 600s


cabal run exe:marlowe-demo -- $ADDR $((1000 * $(date -u +%s)))

yaml2json << EOI > state.json
accounts:
- - - address: $ADDR
    - currency_symbol: ''
      token_name: ''
  - 2000000
boundValues: []
choices: []
minTime: 1
EOI

yaml2json << EOI > metadata.json
1564:
- 1
- - - 5july2023
    - - revision: 1
EOI


marlowe-cli run initialize \
  --roles-currency $POLICY_ID \
  --contract-file contract.json \
  --state-file state.json \
  --out-file tmp.json \
  --merkleize \
  --permanently-without-staking

jq -s '.[0] * {tx : {continuations : .[1] | to_entries | map([.key, .value])}}' tmp.json continuations.json > marlowe-1.json

LAST_TX=$(
marlowe-cli run auto-execute \
  --marlowe-out-file marlowe-1.json \
  --change-address $ADDR \
  --required-signer $SKEY \
  --metadata-file metadata.json \
  --out-file /dev/null \
  --submit 600s \
| sed -e 's/^TxId "\(.*\)"$/\1#1/' \
)
echo $LAST_TX

randomWait

DEPOSIT=$((1 * ADA + RANDOM))
echo $DEPOSIT

marloweChoose 2 $ADDR Amount $DEPOSIT

marloweDeposit 3 $ADDR $DEPOSIT

marloweChoose 4 $(randomRole) Folio $(randomChoice)

marloweChoose 5 $(randomRole) Play $(randomChoice)

marloweChoose 6 $(randomRole) Act $(randomChoice)

marloweChoose 7 $(randomRole) Scene $(randomChoice)

marloweChoose 8 $(randomRole) Line $(randomChoice)

marloweChoose 9 $(randomRole) Word $(randomChoice)

marloweChoose 10 $(randomRole) Letter $(randomChoice)

