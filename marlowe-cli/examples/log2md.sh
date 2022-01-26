#!/usr/bin/env bash

DIR=$(dirname "$0")

SCRIPT="$1"
MD="$2"
LOG="${MD%%.md}".log
DIFF="${MD%%.md}".diff

set -e
bash -o verbose "$SCRIPT" >& "$LOG"
set +e

diff --new-line-format='+%L'       \
     --old-line-format='-%L'       \
     --unchanged-line-format=' %L' \
     "$SCRIPT" "$LOG" > "$DIFF"

gawk -f "$DIR/log2md.awk" "$DIFF"> "$MD"
