#!/usr/bin/env bash

set -eo pipefail

podman network connect --ip "$(cat cluster-b.ip)" rollback cluster-b
