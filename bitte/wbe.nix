# Needed variables:
#   NOMAD_PORT_wbe
#   NOMAD_ALLOC_DIR # with node socket at $NOMAD_ALLOC_DIR/node.sock
{ writeShellScriptBin, cardano-wallet, coreutils, wait-for-socket, lib, network }:
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-wallet wait-for-socket ]}"

  wait-for-socket "$NOMAD_ALLOC_DIR/node.sock"

  # Ugly ugly hack to kill the WBE every hour
  cardano-wallet serve --listen-address '*' --port "$NOMAD_PORT_wbe" --node-socket "$NOMAD_ALLOC_DIR/node.sock" --testnet ${network.networkConfig.ByronGenesisFile} --log-level DEBUG&
  sleep 10800&
  wait -n
  exit 1

''
