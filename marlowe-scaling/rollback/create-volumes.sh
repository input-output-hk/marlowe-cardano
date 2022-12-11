#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

podman volume create rollback_db
podman volume create rollback_ipc

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
ln -s "${IPC}/node-spo-1.socket" "${ROLLBACK_ROOT}/cluster-a.socket"
ln -s "${IPC}/node-spo-9.socket" "${ROLLBACK_ROOT}/cluster-b.socket"
