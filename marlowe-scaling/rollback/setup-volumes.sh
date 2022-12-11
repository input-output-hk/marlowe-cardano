#!/usr/bin/env bash

set -eo pipefail

ROLLBACK_ROOT="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"


IPC="$(podman volume inspect rollback_ipc | jq -r '.[0].Mountpoint')"
echo full > "${IPC}/mode"


CONFIG="${IPC}/config"
mkdir -p "${CONFIG}"
cp -pr "${ROLLBACK_ROOT}"/config/* "${CONFIG}/"
chmod go-rwx "${CONFIG}"/node-spo-?/*.skey

for i in $(seq 1 6)
do
  cat "${ROLLBACK_ROOT}/cluster-a.ip" > "${CONFIG}/node-spo-${i}/host"
done
for i in $(seq 7 9)
do
  cat "${ROLLBACK_ROOT}/cluster-b.ip" > "${CONFIG}/node-spo-${i}/host"
done

echo '{"Producers": [' > "${CONFIG}/topology.json"
for i in $(seq 1 9)
do
  if [[ i -ne 1 ]]
  then
    echo "  ," >> "${CONFIG}/topology.json"
  fi
  echo '  { "addr": "'"$(cat "${CONFIG}/node-spo-${i}/host")"'", "port": '"$(cat "${CONFIG}/node-spo-${i}/port")"', "valency": 1 }' >> "${CONFIG}/topology.json"
done
echo ']}' >> "${CONFIG}/topology.json"

jq 'del(.Producers[6,7,8])' "${CONFIG}/topology.json" > "${CONFIG}/topology-a.json"
jq 'del(.Producers[0,1,2,3,4,5])' "${CONFIG}/topology.json" > "${CONFIG}/topology-b.json"

for i in $(seq 1 9)
do
  jq 'del(.Producers['$((i-1))'])' "${CONFIG}/topology.json" > "${CONFIG}/node-spo-${i}/topology-full.json"
done

for i in $(seq 1 6)
do
  jq 'del(.Producers['$((i-1))'])' "${CONFIG}/topology-a.json" > "${CONFIG}/node-spo-${i}/topology-frag.json"
done
for i in $(seq 7 9)
do
  jq 'del(.Producers['$((i-7))'])' "${CONFIG}/topology-b.json" > "${CONFIG}/node-spo-${i}/topology-frag.json"
done


START_TIME="$(date -d "now + 30 seconds" +%s)"
sed -i -e "s/1669843043/${START_TIME}/" "${CONFIG}/byron-genesis.json"
sed -i -e "s/2022-11-30T21:17:23/$(date -d @"${START_TIME}" -u +%FT%T)/" "${CONFIG}/shelley-genesis.json"


DATA="$(podman volume inspect rollback_db | jq -r '.[0].Mountpoint')"
cp -p "${ROLLBACK_ROOT}"/scripts/run-*.sh "${DATA}/"
