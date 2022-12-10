#!/usr/bin/env bash

set -eo pipefail

podman network disconnect rollback cluster-b
