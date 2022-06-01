# Needed variables:
#   NOMAD_PORT_wbe
#   NOMAD_ALLOC_DIR # with node socket at $NOMAD_ALLOC_DIR/node.sock
{ writeShellScriptBin, cardano-wallet, coreutils, wait-for-socket, sleep-until-restart-slot, lib, network }:
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-wallet wait-for-socket sleep-until-restart-slot ]}"

  wait-for-socket "$NOMAD_ALLOC_DIR/node.sock"

  cardano-wallet serve --listen-address '*' --port "$NOMAD_PORT_wbe" --node-socket "$NOMAD_ALLOC_DIR/node.sock" --testnet ${network.networkConfig.ByronGenesisFile} --log-level DEBUG&
  sleep-until-restart-slot&
  wait -n
  exit 1
''
