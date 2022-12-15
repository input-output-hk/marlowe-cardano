#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
echo frag > "${IPC}/mode"

for c in a b
do
  # FIXME: Find a better way to ensure the constancy of the cluster's IP address.
  podman network disconnect rollback "cluster-${c}"
  podman network connect rollback "cluster-${c}" --ip "$(cat "${ROLLBACK_ROOT}/cluster-${c}.ip")"
  podman restart "cluster-${c}"
done
