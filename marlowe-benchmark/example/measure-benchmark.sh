#!/usr/bin/env bash

set -eo pipefail

echo "PODNAME=$PODNAME"
echo "NETWORK=$NETWORK"
echo "BENCHMARK_CONFIG=$BENCHMARK_CONFIG"
echo "MARLOWE_RT_HOST=$MARLOWE_RT_HOST"
echo "MARLOWE_RT_PORT=$MARLOWE_RT_PORT"
echo "CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH"
echo "CARDANO_TESTNET_MAGIC=$CARDANO_TESTNET_MAGIC"
if [ -z "$CARDANO_TESTNET_MAGIC" ]
then
  MAGIC=(--mainnet)
else
  MAGIC=(--testnet-magic "$CARDANO_TESTNET_MAGIC")
fi
echo "FAUCET_ADDRESS=$FAUCET_ADDRESS"
echo "FAUCET_SKEY=$FAUCET_SKEY"
echo "BENCHMARK_PODSTAT_FILE=$BENCHMARK_PODSTAT_FILE"
echo "BENCHMARK_RESULT_FILE=$BENCHMARK_RESULT_FILE"
echo
podman --version
cardano-cli --version | head -n 1
marlowe-benchmark --version
jq --version
dasel --version
gawk --version |& head -n 1
echo

echo "Starting pod "
podman pod start "$PODNAME"
sleep 90s
while ! cardano-cli query tip "${MAGIC[@]}" >& /dev/null
do
  echo "Waiting for node socket"
  sleep 5s
done
echo

rm "$BENCHMARK_PODSTAT_FILE" 2> /dev/null || true
podman pod stats --format json "$PODNAME" > "$BENCHMARK_PODSTAT_FILE" &
STATS_PID=$!
trap 'kill $STATS_PID' EXIT

marlowe-benchmark \
  --host "$MARLOWE_RT_HOST" \
  --port "$MARLOWE_RT_PORT" \
  --config "$BENCHMARK_CONFIG" \
  --node-socket-path "$CARDANO_NODE_SOCKET_PATH" \
  "${MAGIC[@]}" \
  --address "$FAUCET_ADDRESS" \
  --signing-key-file "$FAUCET_SKEY" \
  --out-file "$BENCHMARK_RESULT_FILE"

for m in Sync BulkSync HeaderSync Query Lifecycle
do
  jq -s 'map(select(.metric == "'"$m"'"))' "$BENCHMARK_RESULT_FILE" \
  | dasel -r json -w csv --csv-comma $'\t' \
  > "$m.tsv"
done

START=$(stat -c %W "$BENCHMARK_PODSTAT_FILE")
for s in "postgres" "node" "chain-indexer" "chain-sync" "contract" "marlowe-indexer" "marlowe-sync" "node" "proxy" "tx" "web-server"
do
  jq -r '.[] | select(.Name | contains("-'"$s"'")) | ((.CPU | sub("%"; "")) + " " +  (.MemUsage | sub(" .*$";"")))' "$BENCHMARK_PODSTAT_FILE" \
    | gawk 'BEGIN {OFS="\t"; print "Epoch", "CPU [%/100]", "Memory [MB]"} {t = '"$START"'+NR-1; p=$1/100} /k/{print t, p, $2 / 1000} /M/{print t, p, $2 * 1} /G/{print t, p, $2 * 1000}' \
  > "benchmark-$s.tsv"
done
