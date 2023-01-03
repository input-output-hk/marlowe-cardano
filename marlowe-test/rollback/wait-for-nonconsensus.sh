#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

while true
do
  if ! "${ROLLBACK_ROOT}/check-consensus.sh"
  then
    exit
  fi
  sleep 1s
done
