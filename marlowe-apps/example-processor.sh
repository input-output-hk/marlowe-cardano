#!/usr/bin/env bash

set -eo pipefail

# Read the input.
INPUT=$(cat)

# Extract the name of the choice.
SYMBOL=$(echo $INPUT | jq -r .symbol)

# Extract the minimum bound.
MIN=$(echo $INPUT | jq -r .bounds[0].from)

# Extract the maximum bound.
MAX=$(echo $INPUT | jq -r .bounds[0].to)

if [[ "$SYMBOL" == "RANDOM.ORG" ]]
then
  # Fetch the data.
  curl 'https://www.random.org/integers/?format=plain&col=1&rnd=new&base=10&num=1&min='$MIN'&max='$MAX
else
  # Report an error.
  echo "Cannot fetch value for symbol: $SYMBOL."
  exit 1
fi
