#!/usr/bin/env bash

cabal run exe:marlowe-cli -- validator --network-magic 1097911063   \
                                       --validator-file test.plutus \
                                       --print-address              \
                                       --print-hash                 \
                                       --print-stats

cabal run exe:marlowe-cli -- datum --account-hash d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657 \
                                   --account-value 3000000                                                 \
                                   --min-slot 10                                                           \
                                   --datum-file test.datum                                                 \
                                   --print-hash                                                            \
                                   --print-stats

cabal run exe:marlowe-cli -- redeemer --min-slot 1000     \
                                      --max-slot 43000000 \
                                      --redeemer-file     \
                                      test.redeemer       \
                                      --print-stats
