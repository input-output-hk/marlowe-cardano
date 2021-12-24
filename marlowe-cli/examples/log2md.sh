#!/usr/bin/env bash

DIR=$(dirname "$0")

SCRIPT="$1"
LOG="${SCRIPT%%.sh}".log
DIFF="${SCRIPT%%.sh}".diff
MD="$2"

bash -o verbose "$SCRIPT" >& "$LOG"

diff --new-line-format='+%L'       \
     --old-line-format='-%L'       \
     --unchanged-line-format=' %L' \
     "$SCRIPT" "$LOG" > "$DIFF"

gawk -f "$DIR/log2md.awk" "$DIFF"> "$MD"
