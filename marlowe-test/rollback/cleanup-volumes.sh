#!/usr/bin/env bash

set -eo pipefail

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
pushd "${IPC}" > /dev/null
rm -rf config node-spo-?.socket
popd > /dev/null

DATA="$(podman volume inspect rollback_db | jq -r '.[0].Mountpoint')"
pushd "${DATA}" > /dev/null
rm -rf node-spo-?.{db,log} run-*.sh
popd > /dev/null
