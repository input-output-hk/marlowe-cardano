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
  -s, --node-socket-path FILE
          Falls back to the CARDANO_NODE_SOCKET_PATH env variable.
          We fallback to \`./node.socket\` when as a least resort.
  --faucet-vkey-file FILE
          File containing the verification key.
  --faucet-skey-file FILE
          File containing the faucet siging key . If this both files are missing
          then new pair of keys will be generated and written down.
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
FAUCET_SKEY_FILE=
FAUCET_VKEY_FILE=

PARSED_ARGUMENTS=$(getopt -a -n "marlowe-cli/run-tests.sh" -o "m:s:fbw:p:vh" --long "testnet-magic:,node-socket-path:,faucet-vkey-file:,faucet-skey-file:,fund,build,wallet-url:,pab-url:,verbose,help" -- "$@")
VALID_ARGUMENTS=$?
if [ "$VALID_ARGUMENTS" != "0" ]; then
  echo "$USAGE"
fi
eval set -- "$PARSED_ARGUMENTS"

while :
do
    case "$1" in
      -m | --testnet-magic)             TESTNET_MAGIC="$2";       shift 2 ;;
      -s | --node-socket-path)          NODE_SOCKET_PATH="$2";    shift 2 ;;
           --faucet-vkey-file)          FAUCET_VKEY_FILE="$2";    shift 2 ;;
           --faucet-skey-file)          FAUCET_SKEY_FILE="$2";    shift 2 ;;
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
  TEST_CASES=(wait refund simple follower-non-empty-payouts-initialization follower-notifies-about-payout-redemption companion-notifications wallet-failure escrow escrow-with-collateral zero-coupon-bond zero-coupon-bond-too-late zero-coupon-bond-immediate-timeout coupon-bond-guaranteed contract-for-differences contract-for-differences-with-oracle swap-of-ada-for-ada follower-notifies-about-already-closed-contract-without-payouts follower-notifies-about-closing-contract-without-payouts)
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

if [[ -z "$FAUCET_SKEY_FILE" ]]; then
  echo "Missing --faucet-skey-file"
  echo "$USAGE"
  exit 1
fi

if [[ -z "$FAUCET_VKEY_FILE" ]]; then
  echo "Missing --faucet-vkey-file"
  echo "$USAGE"
  exit 1
fi

# Create the payment signing and verification keys if they do not already exist.
if [[ ! -e "$FAUCET_SKEY_FILE" && ! -e "$FAUCET_VKEY_FILE" ]]
then
  cardano-cli address key-gen --signing-key-file "$FAUCET_SKEY_FILE"      \
                              --verification-key-file "$FAUCET_VKEY_FILE"
elif [[ ! -e "$FAUCET_SKEY_FILE" || ! -e "$FAUCET_VKEY_FILE" ]]; then
  echo "You should provide both faucet key files or drop both of them if you want to recreate them."
  exit 1
fi

FAUCET_ADDR=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$FAUCET_VKEY_FILE")

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
        --faucet-key "$FAUCET_SKEY_FILE"          \
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

