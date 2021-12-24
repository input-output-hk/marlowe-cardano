#!/usr/bin/env bash

set -xe

for n in everything-is-alright confirm-problem dismiss-claim confirm-claim
do
  ../log2md.sh run-$n.sh $n.md
done
