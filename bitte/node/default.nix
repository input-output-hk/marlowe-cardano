# Needed variables:
#   NODE_STATE_DIR
#   NOMAD_ALLOC_DIR # node socket stored in $NOMAD_ALLOC_DIR/node.sock
#   NOMAD_PORT_node
{ writeShellScriptBin, cardano-node, coreutils, lib, env }:
let
  topology = builtins.toFile "topology.json" (builtins.toJSON {
    Producers = [{
      addr = env.relaysNew;
      port = env.edgePort;
      valency = 1;
    }];
  });

  config = builtins.toFile "config.json" (builtins.toJSON env.nodeConfig);
in
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils cardano-node ]}"

  mkdir -p "$NODE_STATE_DIR"

  exec cardano-node run --topology ${topology} --database-path "$NODE_STATE_DIR/db" --socket-path "$NOMAD_ALLOC_DIR/node.sock" --config ${config} --port "$NOMAD_PORT_node"
''
