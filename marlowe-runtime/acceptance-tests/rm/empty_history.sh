#!/usr/bin/env bash

marlowe-cli --version

cardano-cli --version

git rev-parse HEAD

VALID_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2#1
INVALID_CONTRACT_ID=06b5a9fe7e9868648671333ee1a5ece61af9019b12251b68f1e9fc01cd7a12b2
MISSING_CONTRACT_ID=
EMPTY_STRING_CONTRACT_ID=""

# marlowe rm --all not implemented yet

marlowe rm $VALID_CONTRACT_ID

marlowe ls

marlowe rm $VALID_CONTRACT_ID

marlowe ls

marlowe add $INVALID_CONTRACT_ID

marlowe add $MISSING_CONTRACT_ID

marlowe add $EMPTY_STRING_CONTRACT_ID
