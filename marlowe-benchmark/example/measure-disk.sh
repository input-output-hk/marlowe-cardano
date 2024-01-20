#!/usr/bin/env bash

set -eo pipefail

echo "DISK_FILE=$DISK_FILE"

NODE_DB=$(du -s node.db | sed -e 's/\t.*//')
PG_DB=$(sudo du -s pg.db | sed -e 's/\t.*//')
TAB=$'\t'
cat << EOI > "$DISK_FILE"
Database${TAB}Size [B]
cardano-node${TAB}$NODE_DB
postgresql${TAB}$PG_DB
EOI
