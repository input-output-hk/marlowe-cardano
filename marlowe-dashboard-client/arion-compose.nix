{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) plutus-chain-index cardano-cli marlowe-pab marlowe-cli networks;
  network = networks.testnet-dev;
  marlowe-run-backend-invoker = pkgs.marlowe-dashboard.marlowe-run-backend-invoker;
  node-port = "3001";
  wallet-port-int = 8090;
  wallet-port = toString wallet-port-int;
  chain-index-port-int = 9083;
  chain-index-port = toString chain-index-port-int;
  pab-port = "9080";
  run-port = "8080";
  socket-path = "/ipc/node.socket";
  network-id = toString network.magic;
  pab-params = {
    dbConfigFile = "/data/pab.db";
    baseUrl = "http://0.0.0.0:${pab-port}";
    walletUrl = "http://wallet:${wallet-port}";
    inherit socket-path network;
    protocol-parameters = "./testnet.protocol";
  };
  run-params = { wallet-port = wallet-port-int; chain-index-port = chain-index-port-int; };
  config = pkgs.runCommand "config"
    { }
    ''
      mkdir $out
      echo '${builtins.toJSON (import ./dev/node-config.nix network.nodeConfig)}' > $out/config.json
      echo '${builtins.toJSON (import ./dev/pab-config.nix pab-params)}' > $out/pab.yaml
      echo '${builtins.toJSON (import ./dev/marlowe-run.nix run-params)}' > $out/marlowe-run.json
      cp ${network.networkConfig.AlonzoGenesisFile} $out/alonzo-genesis.json
      cp ${network.networkConfig.ByronGenesisFile} $out/byron-genesis.json
      cp ${network.networkConfig.ShelleyGenesisFile} $out/shelley-genesis.json
      cp ${builtins.toJSON network.topology} $out/topology.yaml
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
      entrypoint = "bash";
      command = [
        "-c"
        ''
          cardano-node run \
            --config /config/config.json \
            --topology /config/topology.yaml \
            --port ${node-port} \
            --socket-path ${socket-path} \
            --database-path /data \
            --host-addr 0.0.0.0
        ''
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
      depends_on = [ "node" ];
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
        "--listen-address"
        "0.0.0.0"
      ];
    };
  };

  chain-index = {
    service = {
      useHostStore = true;
      ports = [ "${chain-index-port}:${chain-index-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "chain-index-data:/data"
      ];
      restart = "on-failure";
      depends_on = [ "wallet" ];
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
          ${plutus-chain-index}/bin/plutus-chain-index start-index \
            --network-id ${network-id} \
            --db-path /data/chain-index.sqlite \
            --socket-path ${socket-path} \
            --port ${chain-index-port}
        ''
      ];
    };
  };

  pab = {
    service = {
      useHostStore = true;
      ports = [ "${pab-port}:${pab-port}" ];
      volumes = [
        "cardano-ipc:/ipc"
        "pab-data:/data"
        "${config}:/config"
      ];
      restart = "on-failure";
      depends_on = [ "chain-index" "wallet" "node" ];
      command = [
        "${pkgs.bash}/bin/bash"
        "-c"
        ''
          set -eu
          die () {
            exit "$$1"
          }
          trap 'die $$?' EXIT
          if ! stat ${pab-params.dbConfigFile} 2> /dev/null; then
            ${marlowe-pab}/bin/marlowe-pab migrate --config /config/pab.yaml
          fi
          # TODO this would be nicer implemented as a healthcheck, but I
          # couldn't get that to work.
          until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${socket-path} 2> /dev/null; do :; done
          CARDANO_NODE_SOCKET_PATH=${socket-path} ${cardano-cli}/bin/cardano-cli query \
            protocol-parameters \
            --testnet-magic ${network-id} \
            --out-file ./testnet.protocol
          ${marlowe-pab}/bin/marlowe-pab webserver \
            --config /config/pab.yaml \
            --passphrase fixme-allow-pass-per-wallet \
            --memory
        ''
      ];
    };
  };

  marlowe-run = {
    service = {
      useHostStore = true;
      ports = [ "${run-port}:${run-port}" ];
      volumes = [ "${config}:/config" ];
      restart = "on-failure";
      depends_on = [ "wallet" ];
      command = [
        "${marlowe-run-backend-invoker}/bin/marlowe-dashboard-server"
        "webserver"
        "--config"
        "/config/marlowe-run.json"
        "--bind"
        "0.0.0.0"
        "--port"
        run-port
        "--network-id"
        network-id
        "--verbosity"
        "2"
      ];
    };
  };

in
{
  config.services = {
    inherit node;
    inherit wallet;
    inherit chain-index;
    inherit pab;
    inherit marlowe-run;
  };
  config.docker-compose.raw = {
    volumes = {
      cardano-ipc = { };
      cardano-node-data = { };
      cardano-wallet-data = { };
      chain-index-data = { };
      pab-data = { };
    };
  };
}
