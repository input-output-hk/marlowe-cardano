#!/usr/bin/env bash

USAGE=\
'USAGE:
  run-tests.sh [OPTIONS] [TEST_NAME]

ARGS:
  <TEST_NAME> Test to execute. Test name is a part of yaml file from `marlowe-cli/test` dir
              without `test-` prefix and `.yaml` suffix. Tests are executed in order.
              If no tests are provided runner is going to execute the whole suite.
OPTIONS:
  -m, --testnet-magic <CARDANO_TESTNET_MAGIC>
          Falls back to the $CARDANO_TESTNET_MAGIC env variable.
          Default is 1567.
  -s, --node-socket-path
          Falls back to the $CARDANO_NODE_SOCKET_PATH env variable.
          We fallback to `./node.socket` when as a least resort.
  -t, --treasury
          Directory which should be used as a store for test suite treasury account.
          We expect two files there: `payment.skey`, `payment.vkey`
          If given directory is empty this runner will create them.
          We fallback to $TREASURY env variable and to `./` at the end.
  -f, --fund
          Whether to fund the testing treasury.
  -b, --build
          Execute test suite by using `cabal run exe:marlowe-cli`
  -w, --wallet-url
          URL to the cardano-wallet. Default is `http://localhost:8090`
  -p, --pab-url
          URL to the marlowe-pab server. Default is `http://localhost:9080`
  -v, --verbose
  -h, --help
'

set -e

BUILD=
VERBOSE=
NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH
TESTNET_MAGIC=$CARDANO_TESTNET_MAGIC
TREASURY=$TREASURY
WALLET_URL=
PAB_URL=
FUND=

PARSED_ARGUMENTS=$(getopt -a -n "marlowe-cli/run-tests" -o "m:s:bvt:fh" --long "testnet-magic:,node-socket-path:,build,verbose,treasury:,fund,help" -- "$@")
VALID_ARGUMENTS=$?
if [ "$VALID_ARGUMENTS" != "0" ]; then
  echo "$USAGE"
fi
eval set -- "$PARSED_ARGUMENTS"

while :
do
    case "$1" in
      -s | --node-socket-path)          NODE_SOCKET_PATH="$2";  shift 2 ;;
      -f | --fund)                      FUND=1;                 shift   ;;
      -n | --testnet-magic)             TESTNET_MAGIC="$2";     shift 2 ;;
      -w | --wallet-url)                WALLET_URL="$2";        shift 2 ;;
      -p | --pab-url)                   PAB_URL="$2";           shift 2 ;;
      -t | --treasury)                  TREASURY="$2";          shift 2 ;;
      -b | --build)                     BUILD=1;                shift   ;;
      -v | --verbose)                   VERBOSE=1;              shift   ;;
      -h | --help)                      echo "$USAGE";          exit  0 ;;
      --) shift; break ;;
      *) echo "Unrecognized option: $1" 1>&2; echo $USAGE; exit 1;;
    esac
done

if [[ -z "$TESTNET_MAGIC" ]]; then
  TESTNET_MAGIC=1567
fi

if [[ -z "$NODE_SOCKET_PATH" ]]; then
  NODE_SOCKET_PATH="./node.socket"
fi

if [[ -z $TREASURY ]]; then
  TREASURY="./"
fi

if [[ -z $WALLET_URL ]]; then
  WALLET_URL="http://localhost:8090"
fi

if [[ -z $PAB_URL ]]; then
  PAB_URL="http://localhost:9080"
fi

FAUCET_SKEY="$TREASURY"/payment.skey
FAUCET_VKEY="$TREASURY"/payment.vkey

# Create the payment signing and verification keys if they do not already exist.
if [[ ! -e "$FAUCET_SKEY" ]]
then
  echo "CREATE"
  cardano-cli address key-gen --signing-key-file "$FAUCET_SKEY"      \
                              --verification-key-file "$FAUCET_VKEY"
fi
FAUCET_ADDRESS=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$FAUCET_VKEY")

if [[ ! -z "$FUND" ]]; then
  # Fund the faucet with 5k tADA.
  marlowe-cli util faucet --testnet-magic "$TESTNET_MAGIC"                   \
                          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                          --out-file /dev/null                      \
                          --submit 600                              \
                          --lovelace 5000000000                     \
                          "$FAUCET_ADDRESS"                         \
  > /dev/null
fi

BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7

# The PAB passphrase must match the `--passphrase` argument of `marlowe-pab`.
PAB_PASSPHRASE=fixme-allow-pass-per-wallet

if [[ -z "$@" ]]; then
  TEST_CASES="companion-notifications-for-two-contracts wait refund wallet-failure simple escrow escrow-with-collateral zero-coupon-bond zero-coupon-bond-too-late zero-coupon-bond-immediate-timeout coupon-bond-guaranteed contract-for-differences contract-for-differences-with-oracle swap-of-ada-for-ada follower-non-empty-payouts-initialization follower-notifies-about-payout-redemption"
else
  TEST_CASES="$@"
fi

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
        --faucet-key "$FAUCET_SKEY"               \
        --faucet-address "$FAUCET_ADDRESS"        \
        --burn-address "$BURN_ADDRESS"            \
        --passphrase "$PAB_PASSPHRASE"            \
         "$1"
  return $?
}


for TEST_CASE in $TEST_CASES
do
  TEST_FILE="./test/test-"$TEST_CASE".yaml"
  if [[ ! -e $TEST_FILE ]]; then
      echo "Given test file doesn't exist: $TEST_FILE. Skipping."
      continue
  fi

  if [ -z "$VERBOSE" ]; then
    echo -n "$TEST_CASE: "
    if runTest "$TEST_FILE" >& "${TEST_FILE%%.yaml}".log
    then
      echo "PASS"
    else
      echo "FAIL"
    fi
  else
    runTest $TEST_FILE | tee "${TEST_FILE%%.yaml}".log
  fi
done

