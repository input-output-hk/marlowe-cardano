#!/bin/bash

set -eo pipefail

IPC=/ipc

CONFIG="${IPC}/config"
mkdir -p "${CONFIG}"

DATA=/data

mode="$(cat ${IPC}/mode)"

for i in $(seq 1 6)
do
  cardano-node run \
    --database-path                   "${DATA}/node-spo-${i}.db" \
    --socket-path                     "${IPC}/node-spo-${i}.socket" \
    --config                          "${CONFIG}/configuration.yaml" \
    --topology                        "${CONFIG}/node-spo-${i}/topology-${mode}.json" \
    --shelley-kes-key                 "${CONFIG}/node-spo-${i}/kes.skey" \
    --shelley-vrf-key                 "${CONFIG}/node-spo-${i}/vrf.skey" \
    --byron-delegation-certificate    "${CONFIG}/node-spo-${i}/byron-delegation.cert" \
    --byron-signing-key               "${CONFIG}/node-spo-${i}/byron-delegate.key" \
    --shelley-operational-certificate "${CONFIG}/node-spo-${i}/opcert.cert" \
    --host-addr                       0.0.0.0 \
    --port                            "$(cat "${CONFIG}/node-spo-${i}/port")" \
    >& "${DATA}/node-spo-${i}.log" &
done
wait
