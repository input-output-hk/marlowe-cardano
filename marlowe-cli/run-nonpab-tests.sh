#!/usr/bin/env bash


basename=$(basename "$0")

DEFAULT_MAGIC="1097911063"
DEFAULT_CARDANO_NODE_SOCKET_PATH="./node.socket"
DEFAULT_TREASURY="."

USAGE=$(cat <<USAGE
Run the non-PAB marlowe-cli tests

USAGE:
  $basename OPTIONS

OPTIONS:
  -m, --magic <INT>
          Magic number of the Cardano network.
          Falls back to the CARDANO_TESTNET_MAGIC env variable.
          Default is $DEFAULT_MAGIC (the Cardano public testnet)
  -s, --node-socket-path <PATH>
          Falls back to the CARDANO_NODE_SOCKET_PATH env variable.
          Default is '$DEFAULT_CARDANO_NODE_SOCKET_PATH'
  -t, --treasury <PATH>
          Directory which should be used as a store for test suite treasury accounts.
          We expect two files there: \`payment.skey\`, \`payment.vkey\`
          If given directory is empty this runner will create them.
          Falls back to the TREASURY env variable.
          Default is '$DEFAULT_TREASURY'
  -a, --faucet-address <ADDRESS>
          Address with ADA to fund the tests.
  -p, --faucet-skey-file <PATH>
          Payment signing key for the address above for funding test accounts.
  -h, --help
USAGE
)


die () {
  rc="$1"
  shift
  echo "$basename:" "$@" >&2
  exit "$rc"
}


PARSED_ARGUMENTS=$(getopt -a -n "$basename" -o m:s:t:a:p:h --long magic:,node-socket-path:,treasury:,faucet-address:,faucet-skey-file:,help -- "$@") \
  || die 1 "$USAGE"

eval set -- "$PARSED_ARGUMENTS"

MAGIC="$CARDANO_TESTNET_MAGIC"
OPTHELP=false

while true ; do
  case "$1" in
    -m|--magic) MAGIC="$2"; shift 2 ;;
    -s|--node-socket-path) CARDANO_NODE_SOCKET_PATH="$2"; shift 2 ;;
    -t|--treasury) TREASURY="$2"; shift 2 ;;
    -a|--faucet-address) FAUCET_ADDRESS="$2"; shift 2 ;;
    -p|--faucet-skey-file) FAUCET_SKEY_FILE="$2"; shift 2 ;;
    -h|--help) OPTHELP=true; shift;;
    --) shift; break;;
  esac
done

$OPTHELP && die 0 "$USAGE"
: "${MAGIC:=$DEFAULT_CARDANO_TESTNET_MAGIC}"
: "${CARDANO_NODE_SOCKET_PATH:=$DEFAULT_CARDANO_NODE_SOCKET_PATH}"
: "${TREASURY:=$DEFAULT_TREASURY}"
[ -z "$FAUCET_ADDRESS" ] && die 1 "Missing required --faucet-address"
[ -z "$FAUCET_SKEY_FILE" ] && die 1 "Missing required --faucet-skey-file"

# These will be needed by the test scripts we're about to run

export MAGIC
export CARDANO_NODE_SOCKET_PATH
export TREASURY
export FAUCET_ADDRESS
export FAUCET_SKEY_FILE

for t in {test/double-satisfaction.sh,examples/*/run-*.sh}
do
  if bash -ve "$t" >& "${t%%.sh}.log"
    then echo "PASS: $t"
    else echo "FAIL: $t"
  fi
done
