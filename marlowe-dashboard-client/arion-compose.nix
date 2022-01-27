{ lib
, pkgs
, ...
}:
let
  node-port = "3001";
  wallet-port = "8090";
  socket-path = "/ipc/node.socket";
  config = pkgs.runCommand "config"
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

  node = {
    service = {
      image = "inputoutput/cardano-node:1.33.0";
      ports = [ "${node-port}:${node-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "cardano-node-data:/data"
        "${config}:/config"
      ];
      command = [
        "run"
        "--config"
        "/config/config.json"
        "--topology"
        "/config/topology.yaml"
        "--port"
        node-port
        "--socket-path"
        socket-path
        "--database-path"
        "/data"
      ];
    };
  };

  wallet = {
    service = {
      image = "inputoutput/cardano-wallet:2021.12.15";
      ports = [ "${wallet-port}:${wallet-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "cardano-wallet-data:/data"
        "${config}:/config"
      ];
      command = [
        "serve"
        "--testnet"
        "/config/byron-genesis.json"
        "--database"
        "/data"
        "--node-socket"
        socket-path
        "--port"
        wallet-port
      ];
    };
  };

in
{
  config.services = {
    inherit node;
    inherit wallet;
  };
  config.docker-compose.raw = {
    volumes = {
      cardano-node-data = { };
      cardano-wallet-data = { };
      cardano-ipc = { };
    };
  };
}
