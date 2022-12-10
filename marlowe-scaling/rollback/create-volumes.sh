#!/usr/bin/env bash

set -eo pipefail

podman volume create rollback_db
podman volume create rollback_ipc

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
ln -s "${IPC}/node-spo-1.socket" cluster-a.socket
ln -s "${IPC}/node-spo-9.socket" cluster-b.socket
