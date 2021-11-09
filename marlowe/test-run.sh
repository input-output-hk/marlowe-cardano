#!/usr/bin/env bash

cabal run exe:marlowe-cli -- validator --network-magic 1097911063   \
                                       --validator-file test.plutus \
                                       --print-address              \
                                       --print-hash                 \
                                       --print-stats
