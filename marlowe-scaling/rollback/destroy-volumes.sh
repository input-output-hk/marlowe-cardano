#!/usr/bin/env bash

set -eo pipefail

podman volume rm rollback_db
podman volume rm rollback_ipc

rm cluster-a.socket
rm cluster-b.socket
