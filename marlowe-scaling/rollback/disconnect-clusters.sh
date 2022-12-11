#!/usr/bin/env bash

set -eo pipefail

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
echo frag > "${IPC}/mode"

for c in a b
do
  # FIXME: Find a better way to ensure the constancy of the cluster's IP address.
  podman network disconnect rollback "cluster-${c}"
  podman network connect rollback "cluster-${c}" --ip "$(cat cluster-${c}.ip)"
  podman restart "cluster-${c}"
done
