#!/usr/bin/env bash

set -eo pipefail

IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
export CARDANO_NODE_SOCKET_PATH="${IPC}/node-spo-9.socket"

watch "cardano-cli query tip --testnet-magic 1564 | jq '{cluster:"'"b"'",tip:.}' | json2yaml"
