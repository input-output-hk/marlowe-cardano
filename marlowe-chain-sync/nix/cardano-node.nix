{ pkgs, network, port }:
let
  cardano-node = pkgs.project.hsPkgs.cardano-node.components.exes.cardano-node;
  config = pkgs.writeTextFile {
    name = "node-config.json";
    text = builtins.toJSON
      (import
        ../../marlowe-dashboard-client/dev/node-config.nix
        { config = network.nodeConfig; }
      );
  };
  socket-path = "/tmp/node.socket";
  database-path = "db/node.db";
in
pkgs.writeShellScriptBin "run-node" ''
  mkdir -p ${database-path}
  ${cardano-node}/bin/cardano-node run --config ${config} \
                                       --topology ${network.topology} \
                                       --port ${toString port} \
                                       --socket-path ${socket-path} \
                                       --database-path ${database-path}
''
