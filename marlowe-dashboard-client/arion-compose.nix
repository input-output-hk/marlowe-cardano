{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) cardano-wallet cardano-node;
  node-port = "3001";
  wallet-port = "8090";

  cardano-node-service = {
    image = {
      name = "cardano-node";
      contents = [ cardano-node ];
      command = [
        "${cardano-node}/bin/cardano-node"
        "run"
        "--config"
        "/var/config/config.json"
        "--topology"
        "/var/config/topology.yaml"
        "--database-path"
        "/var/db"
        "--socket-path"
        "/var/alloc/node.sock"
        "--port"
        "8000"
      ];
    };
    service = {
      useHostStore = true;
      ports = [ "${node-port}:8000" ];
      volumes = [
        "alloc-volume:/var/alloc"
        "${../bitte/node/config}:/var/config"
        "/var/db"
      ];
    };
  };

  cardano-wallet-service = {
    image = {
      name = "cardano-wallet";
      contents = [ cardano-wallet ];
      command = [
        "${cardano-wallet}/bin/cardano-wallet"
        "serve"
        "--listen-address"
        "*"
        "--node-socket"
        "/var/alloc/node.sock"
        "--testnet"
        "/var/config/byron-genesis.json"
        "--port"
        "8000"
      ];
    };
    service = {
      useHostStore = true;
      ports = [ "${wallet-port}:8000" ];
      volumes = [
        "alloc-volume:/var/alloc"
        "${../bitte/node/config}:/var/config"
      ];
    };
  };

in
{
  config.services = {
    inherit cardano-node-service cardano-wallet-service;
  };
  config.docker-compose.raw = {
    volumes = {
      alloc-volume = { };
    };
  };
}
