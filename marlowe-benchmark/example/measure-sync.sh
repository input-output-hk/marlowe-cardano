#!/usr/bin/env bash

set -eo pipefail

echo "--- Environment variables ---"
echo "PODNAME=$PODNAME"
echo "DB_PORT=$DB_PORT"
echo "CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH"
echo "CARDANO_TESTNET_MAGIC=$CARDANO_TESTNET_MAGIC"
if [ -z "$CARDANO_TESTNET_MAGIC" ]
then
  MAGIC=(--mainnet)
else
  MAGIC=(--testnet-magic "$CARDANO_TESTNET_MAGIC")
fi
echo "SYNC_PODSTAT_FILE=$SYNC_PODSTAT_FILE"
echo "SYNC_RESULT_FILE=$SYNC_RESULT_FILE"
echo
echo "--- Tools ---"
podman --version
cardano-cli --version |& head -n 1
psql --version
jq --version
gawk --version |& head -n 1
bc --version |& head -n 1
echo

echo -n "Starting container "
podman start "$PODNAME-postgres"
sleep 30s
echo "Waiting for postgresql"
while ! psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select 1' >& /dev/null
do
  sleep 5s
done
echo

rm "$SYNC_PODSTAT_FILE" 2> /dev/null || true
podman pod stats --format json "$PODNAME" > "$SYNC_PODSTAT_FILE" &
STATS_PID=$!
trap 'kill $STATS_PID' EXIT

echo -n "Starting container "
podman start "$PODNAME-node"
NODE_START=$(date -u +%s)
echo "Waiting for node socket"
while ! cardano-cli query tip "${MAGIC[@]}" >& /dev/null
do
  sleep 5s
done

NODE_SYNC=0
while (( $(echo "$NODE_SYNC < 99.99" | bc -l) ))
do
  NODE_SYNC=$(cardano-cli query tip "${MAGIC[@]}" | jq -r .syncProgress)
  echo -n $'\r'"Node sync: $NODE_SYNC%        "
  sleep 1s
done
echo
echo
NODE_FINISH=$(date -u +%s)

echo -n "Starting container "
podman start "$PODNAME-chain-indexer"
CHAIN_START=$(date -u +%s)
echo "Waiting for chain schema"
while ! psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select max(slotno) from chain.block' >& /dev/null
do
  sleep 1s
done

LAG=10
while [ $LAG -gt 5 ]
do
  NODE_SLOT=$(cardano-cli query tip "${MAGIC[@]}" | jq -r .slot)
  CHAIN_SLOT=$(psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select max(slotno) from chain.block' -tA)
  LAG=$((NODE_SLOT - CHAIN_SLOT))
  echo -n $'\r'"Chain indexer lag: $LAG slots          "
  sleep 1s
done
echo
echo
CHAIN_FINISH=$(date -u +%s)

echo -n "Starting container "
podman start "$PODNAME-chain-sync"
echo

echo -n "Starting container "
podman start "$PODNAME-marlowe-indexer"
MARLOWE_START=$(date -u +%s)
echo "Waiting for marlowe schema"
while ! psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select max(slotno) from marlowe.block' >& /dev/null
do
  sleep 1s
done

LAG=10
while [ $LAG -gt 5 ]
do
  CHAIN_SLOT=$(psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select max(slotno) from chain.block where slotno in (select slotno from marlowe.createtxout natural join chain.txout)' -tA)
  MARLOWE_SLOT=$(psql -h "$(hostname)" -p "$DB_PORT" -U postgres -c 'select max(slotno) from marlowe.block' -tA)
  LAG=$((CHAIN_SLOT - MARLOWE_SLOT))
  echo -n $'\r'"Marlowe indexer lag: $LAG slots          "
  sleep 1s
done
echo
echo
MARLOWE_FINISH=$(date -u +%s)

START=$(stat -c %W "$SYNC_PODSTAT_FILE")
for s in "postgres" "node" "chain-indexer" "chain-sync" "marlowe-indexer"
do
  jq -r '.[] | select(.Name | contains("'"$s"'")) | ((.CPU | sub("%"; "")) + " " +  (.MemUsage | sub(" .*$";"")))' "$SYNC_PODSTAT_FILE" \
    | gawk 'BEGIN {OFS="\t"; print "Epoch", "CPU [%/100]", "Memory [MB]"} {t = '"$START"'+NR-1; p=$1/100} /k/{print t, p, $2 / 1000} /M/{print t, p, $2 * 1} /G/{print t, p, $2 * 1000}' \
  > "sync-$s.tsv"
done

TAB=$'\t'
cat << EOI > "$SYNC_RESULT_FILE"
Service${TAB}Start Epoch${TAB}Finish Epoch
cardano-node${TAB}$NODE_START${TAB}$NODE_FINISH
marlowe-chain-indexer${TAB}$CHAIN_START${TAB}$CHAIN_FINISH
marlowe-indexer${TAB}$MARLOWE_START${TAB}$MARLOWE_FINISH
EOI

echo -n "Starting remaining containers in pod "
podman pod start "$PODNAME"
