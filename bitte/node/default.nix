# Needed variables:
#   NODE_STATE_DIR
#   NOMAD_ALLOC_DIR # node socket stored in $NOMAD_ALLOC_DIR/node.sock
#   NOMAD_PORT_node
{ writeShellScriptBin, cardano-node, coreutils, lib }:
let
  config-dir = ./config;
in
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-node ]}"

  exec cardano-node run --topology ${config-dir}/topology.yaml --database-path "$NODE_STATE_DIR/db" --socket-path "$NOMAD_ALLOC_DIR/node.sock" --config ${config-dir}/config.json  --port "$NOMAD_PORT_node"
''
