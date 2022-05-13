#!/usr/bin/env bash

USAGE=\
"USAGE:
  run-tests.sh [OPTIONS] [TEST_NAME]

ARGS:
  <TEST_NAME> Test to execute. Test name is a part of yaml file from \`marlowe-cli/test\` dir
              without \`test-\` prefix and \`.yaml\` suffix. Tests are executed in order.
              If no tests are provided runner is going to execute the whole suite.

OPTIONS:
  -m, --testnet-magic NATURAL
          Falls back to the CARDANO_TESTNET_MAGIC env variable.
          Default is 1567.
  -S, --node-socket-path FILE
          Falls back to the CARDANO_NODE_SOCKET_PATH env variable.
          We fallback to \`./node.socket\` when as a least resort.
  -t, --treasury DIR
          In the event that faucet keys need to be created, they will be made
          in this directory. We fallback to the TREASURY env variable and to
          \`./\` at the end.
  -a, --faucet-addr-file FILE
          File containing the faucet address
  -s, --faucet-skey-file FILE
          File containing the faucet SKEY. If this file doesn't exist, new
          wallet keys will be created as payment.skey and payment.vkey files in
          the TREASURY dir.
  -f, --fund
          Whether to fund the testing treasury.
  -b, --build
          Execute test suite by using \`cabal run exe:marlowe-cli\`
  -w, --wallet-url URL
          URL to the cardano-wallet. Default is \`http://localhost:8090\`
  -p, --pab-url URL
          URL to the marlowe-pab server. Default is \`http://localhost:9080\`
  -v, --verbose
  -h, --help
"

set -e

BUILD=
VERBOSE=
NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-./node.socket}"
TESTNET_MAGIC="${CARDANO_TESTNET_MAGIC:-1567}"
WALLET_URL="http://localhost:8090"
PAB_URL="http://localhost:9080"
FUND=
TREASURY_DIR="${TREASURY:-./}"
FAUCET_ADDR=
FAUCET_SKEY_FILE=

PARSED_ARGUMENTS=$(getopt -a -n "marlowe-cli/run-tests.sh" -o "m:S:t:a:s:fbW:P:vh" --long "testnet-magic:,node-socket-path:,treasury:,faucet-addr-file:,faucet-skey-file:,fund,build,wallet-url:,pab-url:,verbose,help" -- "$@")
VALID_ARGUMENTS=$?
if [ "$VALID_ARGUMENTS" != "0" ]; then
  echo "$USAGE"
fi
eval set -- "$PARSED_ARGUMENTS"

while :
do
    case "$1" in
      -m | --testnet-magic)             TESTNET_MAGIC="$2";       shift 2 ;;
      -S | --node-socket-path)          NODE_SOCKET_PATH="$2";    shift 2 ;;
      -t | --treasury)                  TREASURY_DIR="$2";        shift 2 ;;
      -a | --faucet-addr-file)          FAUCET_ADDR=$(cat "$2");  shift 2 ;;
      -s | --faucet-skey-file)          FAUCET_SKEY_FILE="$2";    shift 2 ;;
      -f | --fund)                      FUND=1;                   shift   ;;
      -b | --build)                     BUILD=1;                  shift   ;;
      -w | --wallet-url)                WALLET_URL="$2";          shift 2 ;;
      -p | --pab-url)                   PAB_URL="$2";             shift 2 ;;
      -v | --verbose)                   VERBOSE=1;                shift   ;;
      -h | --help)                      echo "$USAGE";            exit  0 ;;
      --) shift; break ;;
      *) echo "Unrecognized option: $1" 1>&2; echo "$USAGE"; exit 1;;
    esac
done

if [[ "${#@}" -eq 0 ]]; then
  TEST_CASES=(wait refund simple follower-non-empty-payouts-initialization follower-notifies-about-payout-redemption companion-notifications-for-two-contracts wallet-failure escrow escrow-with-collateral zero-coupon-bond zero-coupon-bond-too-late zero-coupon-bond-immediate-timeout coupon-bond-guaranteed contract-for-differences contract-for-differences-with-oracle swap-of-ada-for-ada)
else
  TEST_CASES=("$@")
fi

# Report missing test check before running anything.
for TEST_CASE in "${TEST_CASES[@]}"
do
  TEST_FILE="./test/test-$TEST_CASE.yaml"
  if [[ ! -e $TEST_FILE ]]; then
      echo "Given test file doesn't exist: $TEST_FILE."
      exit 1
  fi
done

# Create the payment signing and verification keys if they do not already exist.
if [[ ! -e "$FAUCET_SKEY_FILE" ]]
then
  FAUCET_SKEY_FILE="$TREASURY_DIR/payment.skey"

  if [[ -e $FAUCET_SKEY_FILE ]]; then
    echo "No --faucet-skey-file given but there's already a file at the treasury location $FAUCET_SKEY_FILE. Aborting because we don't want to overwrite your files with 'cardano-cli address key-gen'"
    exit 1
  fi

  FAUCET_VKEY_FILE="$TREASURY_DIR/payment.vkey"
  FAUCET_ADDR_FILE="$TREASURY_DIR/faucet.address"

  echo "Creating $FAUCET_SKEY_FILE, $FAUCET_VKEY_FILE and $FAUCET_ADDR_FILE"

  cardano-cli address key-gen --signing-key-file "$FAUCET_SKEY_FILE"      \
                              --verification-key-file "$FAUCET_VKEY_FILE"

  # We only need to generate this if we created the wallet keys just now.
  FAUCET_ADDR=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$FAUCET_VKEY_FILE")
  # and let's write it to a file for the next time for the -a|--faucet-addr-file switch
  echo "$FAUCET_ADDR" > "$FAUCET_ADDR_FILE"
fi

if [[ -n "$FUND" ]]; then
  # Fund the faucet with 5k tADA.
  marlowe-cli util faucet --testnet-magic "$TESTNET_MAGIC"          \
                          --socket-path "$NODE_SOCKET_PATH" \
                          --out-file /dev/null                      \
                          --submit 600                              \
                          --lovelace 5000000000                     \
                          "$FAUCET_ADDR"                            \
  > /dev/null
fi

BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7

# The PAB passphrase must match the `--passphrase` argument of `marlowe-pab`.
PAB_PASSPHRASE=fixme-allow-pass-per-wallet

if [[ -z "$BUILD" ]];then
  EXEC="marlowe-cli"
else
  EXEC="cabal run exe:marlowe-cli"
fi
runTest() {
  $EXEC -- test contracts                         \
        --testnet-magic "$TESTNET_MAGIC"          \
        --socket-path "$NODE_SOCKET_PATH"         \
        --wallet-url "$WALLET_URL"                \
        --pab-url "$PAB_URL"                      \
        --faucet-key "$FAUCET_SKEY_FILE"               \
        --faucet-address "$FAUCET_ADDR"           \
        --burn-address "$BURN_ADDRESS"            \
        --passphrase "$PAB_PASSPHRASE"            \
         "$1"
  return $?
}

if [[ -n "$VERBOSE" ]]; then
  echo "Running test cases in order: ${TEST_CASES[*]}"
fi

for TEST_CASE in "${TEST_CASES[@]}"
do
  TEST_FILE="./test/test-$TEST_CASE.yaml"
  if [ -z "$VERBOSE" ]; then
    echo -n "$TEST_CASE: "
    if runTest "$TEST_FILE" >& "${TEST_FILE%%.yaml}".log
    then
      echo "PASS"
    else
      echo "FAIL"
    fi
  else
    runTest "$TEST_FILE" |& tee "${TEST_FILE%%.yaml}".log
  fi
done

