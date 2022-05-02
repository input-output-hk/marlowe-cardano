#!/usr/bin/env bash

USAGE=\
"USAGE:
  run-nonpab-tests.sh [OPTIONS]

OPTIONS:
  -m, --testnet-magic <CARDANO_TESTNET_MAGIC>
          Falls back to the $CARDANO_TESTNET_MAGIC env variable.
          Default is 1567.
  -s, --node-socket-path
          Falls back to the $CARDANO_NODE_SOCKET_PATH env variable.
          We fallback to \`./node.socket\` when as a least resort.
  -t, --treasury
          Directory which should be used as a store for test suite treasury account.
          We expect two files there: \`payment.skey\`, \`payment.vkey\`
          If given directory is empty this runner will create them.
          We fallback to $TREASURY env variable and to \`./\` at the end.
  -h, --help
"

set -e

NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH
MAGIC=$CARDANO_TESTNET_MAGIC

PARSED_ARGUMENTS=$(getopt -a -n "marlowe-cli/run-nonpab-tests" -o "m:s:bvt:fh" --long "testnet-magic:,node-socket-path:,build,verbose,treasury:,fund,help" --)
VALID_ARGUMENTS=$?
if [ "$VALID_ARGUMENTS" != "0" ]; then
  echo "$USAGE"
fi
eval set -- "$PARSED_ARGUMENTS"

while :
do
    case "$1" in
      -s | --node-socket-path)          NODE_SOCKET_PATH="$2";  shift 2 ;;
      -n | --testnet-magic)             MAGIC="$2";             shift 2 ;;
      -t | --treasury)                  TREASURY="$2";          shift 2 ;;
      -h | --help)                      echo "$USAGE";          exit  0 ;;
      --) shift; break ;;
      *) echo "Unrecognized option: $1" 1>&2; echo "$USAGE"; exit 1;;
    esac
done

if [[ -z "$MAGIC" ]]; then
  MAGIC=1567
fi

if [[ -z "$NODE_SOCKET_PATH" ]]; then
  NODE_SOCKET_PATH="./node.socket"
fi

if [[ -z $TREASURY ]]; then
  TREASURY="."
fi


export CARDANO_NODE_SOCKET_PATH
export MAGIC
export TREASURY

if bash -ve "test/double-satisfaction.sh" >& "test/double-satisfaction.log"
then
    echo "PASS: test/double-satisfaction.sh"
else
    echo "FAIL: test/double-satisfaction.sh"
fi

for t in examples/*/run-*.sh
do
  if bash -ve "$t" >& "${t%%.sh}.log"
  then
    echo "PASS: $t"
  else
    echo "FAIL: $t"
  fi
done
