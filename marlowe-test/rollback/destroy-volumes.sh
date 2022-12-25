#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

podman volume rm rollback_db
podman volume rm rollback_ipc

rm "${ROLLBACK_ROOT}/cluster-a.socket"
rm "${ROLLBACK_ROOT}/cluster-b.socket"
