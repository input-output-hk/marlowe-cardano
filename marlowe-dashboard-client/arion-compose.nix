{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) plutus-chain-index cardano-cli;
  node-port = "3001";
  wallet-port = "8090";
  chain-index-port = "9083";
  socket-path = "/ipc/node.socket";
  network-id = "1564";
  config = pkgs.runCommand "config"
    { }
    ''
      mkdir $out
      echo '${builtins.toJSON (import ./dev/node-config.nix)}' > $out/config.json
      cp ${../bitte/node/config/alonzo-genesis.json} $out/alonzo-genesis.json
      cp ${../bitte/node/config/byron-genesis.json} $out/byron-genesis.json
      cp ${../bitte/node/config/shelly-genesis.json} $out/shelly-genesis.json
      cp ${../bitte/node/config/topology.yaml} $out/topology.yaml
    '';

  node = {
    service = {
      image = "inputoutput/cardano-node:1.33.0";
      ports = [ "${node-port}:${node-port}" ];
      restart = "on-failure";
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
      restart = "on-failure";
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

  chain-index = {
    image = { };
    service = {
      useHostStore = true;
      ports = [ "${chain-index-port}:${chain-index-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "chain-index-data:/data"
        "${config}:/config"
      ];
      restart = "on-failure";
      command = [
        "${pkgs.bash}/bin/bash"
        "-c"
        ''
          set -eu

          die () {
            exit "$$1"
          }

          trap 'die $$?' EXIT

          # TODO this would be nicer implemented as a healthcheck, but I
          # couldn't get that to work.
          until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${socket-path} 2> /dev/null; do :; done

          CARDANO_NODE_SOCKET_PATH=${socket-path} ${cardano-cli}/bin/cardano-cli query \
            protocol-parameters \
            --testnet-magic ${network-id} \
            --out-file /config/testnet.protocol

          ${plutus-chain-index}/bin/plutus-chain-index start-index \
            --network-id ${network-id} \
            --db-path /data/chain-index.sqlite \
            --socket-path ${socket-path} \
            --port ${chain-index-port}
        ''
      ];
    };
  };

in
{
  config.services = {
    inherit node;
    inherit wallet;
    inherit chain-index;
  };
  config.docker-compose.raw = {
    volumes = {
      cardano-ipc = { };
      cardano-node-data = { };
      cardano-wallet-data = { };
      chain-index-data = { };
    };
  };
}
