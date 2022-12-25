#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

podman network create --disable-dns --subnet "$(cat "${ROLLBACK_ROOT}"/CIDR)" rollback
