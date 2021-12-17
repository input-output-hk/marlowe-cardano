#!/usr/bin/env bash

SCRIPT="$1"
LOG="${SCRIPT%%.sh}".log
DIFF="${SCRIPT%%.sh}".diff
MD="${SCRIPT%%.sh}".md

bash -o verbose "$SCRIPT" >& "$LOG"

diff --new-line-format='+%L'       \
     --old-line-format='-%L'       \
     --unchanged-line-format=' %L' \
     "$SCRIPT" "$LOG" > "$DIFF"

gawk -f log2md.awk "$DIFF"> "$MD"
