#!/usr/bin/env bash

set -e

CONTRACT=example.contract
UNSIGNED=example.txBody
SIGNED=example.tx

MINADA=1500000
SLOTLENGTH=1000

SYMBOL="$1"
ORACLE="$2"
ADDRESS="$3"
SKEY="$4"
TIMEOUT="$((SLOTLENGTH * ($(date -u +%s) + 24 * 3600)))"

cat << EOI > "$CONTRACT"
{
  "when": [
    {
      "case" : {
        "for_choice" : {
          "choice_name" : "$SYMBOL"
        , "choice_owner" : {
            "address" : "$ORACLE"
          }
        }
      , "choose_between": [
          {
            "from" : 0
          , "to" : 1000000000000000000
          }
        ]
      }
    , "then" : "close"
    }
  ]
, "timeout" : $TIMEOUT
, "timeout_continuation" : "close"
}
EOI

echo "Address: $ORACLE"
echo "Symbol: $SYMBOL"
echo "Timeout: $TIMEOUT = $(date -d @$((TIMEOUT / SLOTLENGTH)))"


"$MARLOWE_BIN/marlowe" create \
  --core-file "$CONTRACT" \
  --min-utxo "$MINADA" \
  --change-address "$ADDRESS" \
  --manual-sign "$UNSIGNED" \
| jq fromjson \
| json2yaml

"$MARLOWE_BIN/cardano-cli" transaction sign \
  --tx-body-file "$UNSIGNED" \
  --out-file "$SIGNED" \
  --signing-key-file "$SKEY"

"$MARLOWE_BIN/marlowe" submit \
  "$SIGNED" \
| jq fromjson \
| json2yaml
