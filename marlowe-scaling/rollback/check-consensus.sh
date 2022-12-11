#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

for c in a b
do
  CARDANO_NODE_SOCKET_PATH="${ROLLBACK_ROOT}/cluster-${c}.socket" cardano-cli query tip --testnet-magic 1564 > "cluster-${c}.tip" &
done
wait

if [ "$(md5sum cluster-a.tip | sed -e 's/ .*//')" == "$(md5sum cluster-b.tip | sed -e 's/ .*//')" ]
then
  exit 0
else
  exit 2
fi
