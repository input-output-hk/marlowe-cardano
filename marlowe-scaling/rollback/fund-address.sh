#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
export CARDANO_NODE_SOCKET_PATH="${IPC}/node-spo-1.socket"

TX_IN=$(
cardano-cli query utxo \
  --testnet-magic 1564 \
  --address "$(cat "${ROLLBACK_ROOT}/config/utxo-keys/utxo1.address")" \
  --out-file /dev/stdout \
| jq -r '. | to_entries | .[0].key'
)

cardano-cli transaction build \
  --testnet-magic 1564 \
  --tx-in "${TX_IN}" \
  --tx-out "$1+10000000000" \
  --change-address "$(cat "${ROLLBACK_ROOT}/config/utxo-keys/utxo1.address")" \
  --out-file tx.raw

cardano-cli transaction sign \
  --tx-body-file tx.raw \
  --out-file tx.signed \
  --signing-key-file "${ROLLBACK_ROOT}/config/utxo-keys/utxo1.skey"

cardano-cli transaction submit \
  --testnet-magic 1564 \
  --tx-file tx.signed
