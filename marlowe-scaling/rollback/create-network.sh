#!/usr/bin/env bash

set -eo pipefail

podman network create --disable-dns --subnet "$(cat CIDR)" rollback
