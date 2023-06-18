#!/usr/bin/env bash

set -evo pipefail


. ../../networks/preprod/configure.env

export PATH=$MARLOWE_BIN:$PATH


SKEY=demo.skey
ADDR=$(cat demo.testnet.address)
echo $ADDR

POLICY_ID=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d

ADA=1000000


function randomWait {
  sleep $((10 + RANDOM % 5))s
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

function marloweChoose {
  marlowe-cli run prepare \
    --choice-party "$2" \
    --choice-name "$3" \
    --choice-number "$4" \
    --invalid-before $((1000 * ($(date -u +%s) - 1 * 60))) \
    --invalid-hereafter $((1000 * ($(date -u +%s) + 5 * 60))) \
    --marlowe-file marlowe-$(($1-1)).json \
    --out-file marlowe-$1.json
  marloweSubmit $1
}

function randomPattern {
  case $((RANDOM % 7)) in
    0)
      marloweChoose $1 sound 'bd hh sn hh' $((RANDOM % 4 + 1))
      ;;
    1)
      marloweChoose $1 sound 'bd [bd cp] bd bd' $((RANDOM % 4 + 1))
      ;;
    2)
      marloweChoose $1 sound '<bd sn> <sd [cp cp]> <bd [cp cp]>' $((RANDOM % 4 + 1))
      ;;
    3)
      marloweChoose $1 sound '[kurt kurt] <jungbass industrial>' $((RANDOM % 4 + 1))
      ;;
    4)
      marloweChoose $1 sound 'tabla*3 drum jvbass arpy' $((RANDOM % 4 + 1))
      ;;
    5)
      marloweChoose $1 sound 'supermandolin' $((RANDOM % 4 + 1))
      ;;
    6)
      marloweChoose $1 sound 'superpiano' $((RANDOM % 4 + 1))
      ;;
  esac
}

function randomSample {
  case $((RANDOM % 7)) in
    0)
      marloweChoose $1 'note' 'c a f e'     $((RANDOM % 4 + 1))
      ;;
    1)
      marloweChoose $1 'note' 'c4 a3 f6 e5' $((RANDOM % 4 + 1))
      ;;
    2)
      marloweChoose $1 'n'    '1'           $((RANDOM % 4 + 1))
      ;;
    3)
      marloweChoose $1 'n'    '1 2'         $((RANDOM % 4 + 1))
      ;;
    4)
      marloweChoose $1 'n'    '1 2 3 3'     $((RANDOM % 4 + 1))
      ;;
    5)
      marloweChoose $1 'n'    '1 2 3 4 '    $((RANDOM % 4 + 1))
      ;;
    6)
      marloweChoose $1 'n'    '4 2 3 1'     $((RANDOM % 4 + 1))
      ;;
  esac
}

function randomEffect {
  case $((RANDOM % 7)) in
    0)
      marloweChoose $1 'vowel'         'a e o'       $((RANDOM % 4 + 1))
      ;;
    1)
      marloweChoose $1 'gain'          '1 0.7 0.5'   $((RANDOM % 4 + 1))
      ;;
    2)
      marloweChoose $1 'speed'         '1 1.5 2 0.5' $((RANDOM % 4 + 1))
      ;;
    3)
      marloweChoose $1 'delayfeedback' '<0.5 0.9>'   $((RANDOM % 4 + 1))
      ;;
    4)
      marloweChoose $1 'room'          '0.6'         $((RANDOM % 4 + 1))
      ;;
    5)
      marloweChoose $1 'legato'        '1 2'         $((RANDOM % 4 + 1))
      ;;
    6)
      marloweChoose $1 'resonance'     '0.2 0.5'     $((RANDOM % 4 + 1))
      ;;
  esac
}


cardano-cli query utxo ${MAGIC[@]} --address $ADDR

marlowe-cli util clean \
  --lovelace $((2 * ADA)) \
  --change-address $ADDR \
  --required-signer $SKEY \
  --out-file /dev/null \
  --submit 600s


cabal run exe:marlowe-demo -- $((1000 * $(date -u +%s)))

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
- - - tidal
    - - revision: 2
EOI


marlowe-cli run initialize \
  --roles-currency $POLICY_ID \
  --contract-file contract.json \
  --state-file state.json \
  --out-file tmp.json \
  --merkleize \
  --permanently-without-staking

jq -s '.[0] * {tx : {continuations : .[1] | to_entries | map([.key, .value])}}' tmp.json continuations.json > marlowe-1.json

marlowe-cli run auto-execute \
  --marlowe-out-file marlowe-1.json \
  --change-address $ADDR \
  --required-signer $SKEY \
  --metadata-file metadata.json \
  --out-file /dev/null

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

randomPattern 2

randomSample 3

randomEffect 4

randomEffect 5

randomPattern 6

randomSample 7

randomEffect 8

randomEffect 9

