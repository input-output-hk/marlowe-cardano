{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) cardano-wallet;
  node-port = "3001";
  socket-path = "/ipc/node.socket";
  node-config = pkgs.runCommand "node-config"
    { }
    ''
      mkdir $out
      echo '${builtins.toJSON (import ./dev/node-config.nix)}' > $out/config.json
      cp ${../bitte/node/config/alonzo-genesis.json} $out/alonzo-genesis.json
      cp ${../bitte/node/config/byron-genesis.json} $out/byron-genesis.json
      cp ${../bitte/node/config/shelly-genesis.json} $out/shelly-genesis.json
      cp ${../bitte/node/config/topology.yaml} $out/topology.yaml
      ls $out
    '';

  cardano-node = {
    service = {
      image = "cardano-node:1.33.0";
      ports = [ "${node-port}:${node-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "cardano-data:/data"
        "${node-config}:/config"
      ];
      command = [
        "run"
        "--config"
        "/config/config.json"
        "--topology"
        "/config/topology.yaml"
        "--socket-path"
        socket-path
        "--database-path"
        "/data"
      ];
    };
  };

in
{
  config.services = {
    inherit cardano-node;
  };
  config.docker-compose.raw = {
    volumes = {
      cardano-data = { };
      cardano-ipc = { };
    };
  };
}
