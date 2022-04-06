{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) plutus-chain-index cardano-cli marlowe-pab marlowe-cli;
  marlowe-run-backend-invoker = pkgs.marlowe-dashboard.marlowe-run-backend-invoker;
  node-port = "3001";
  wallet-port-int = 8090;
  wallet-port = toString wallet-port-int;
  chain-index-port-int = 9083;
  chain-index-port = toString chain-index-port-int;
  pab-port = "9080";
  run-port = "8080";
  socket-path = "/ipc/node.socket";
  network-id = "1564";
  pab-params = {
    dbConfigFile = "/data/pab.db";
    baseUrl = "http://0.0.0.0:${pab-port}";
    walletUrl = "http://wallet:${wallet-port}";
    inherit socket-path network-id;
    protocol-parameters = "./testnet.protocol";
  };
  run-params = { wallet-port = wallet-port-int; chain-index-port = chain-index-port-int; };
  config = pkgs.runCommand "config"
    { }
    ''
      mkdir $out
      echo '${builtins.toJSON (import ./dev/node-config.nix)}' > $out/config.json
      echo '${builtins.toJSON (import ./dev/pab-config.nix pab-params)}' > $out/pab.yaml
      echo '${builtins.toJSON (import ./dev/marlowe-run.nix run-params)}' > $out/marlowe-run.json
      cp ${../bitte/node/config/alonzo-genesis.json} $out/alonzo-genesis.json
      cp ${../bitte/node/config/byron-genesis.json} $out/byron-genesis.json
      cp ${../bitte/node/config/shelly-genesis.json} $out/shelly-genesis.json
      cp ${../bitte/node/config/topology.yaml} $out/topology.yaml
    '';

  node-seeder = {
    service = {
      useHostStore = true;
      volumes = [
        "cardano-node-data:/data"
        "${./dev}:/copy"
      ];
      command = [
        "${pkgs.bash}/bin/bash"
        "-c"
        ''
          export PATH=$PATH:${pkgs.gzip}/bin
          if [ -z "$$(${pkgs.coreutils}/bin/ls -A /data)" ]; then
            ${pkgs.gnutar}/bin/tar xf /copy/node.db.saved.tar.gz --strip-components 1 -C /data
          fi
        ''
      ];
    };
  };

  node = {
    service = {
      image = "inputoutput/cardano-node:1.33.0";
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
          until [ ! -z "$$(ls -A /data)" ]; do :; done
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
            --verbose
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
            --passphrase fixme-allow-pass-per-wallet
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

  marlowe-cli-tests = {
    service = {
      useHostStore = true;
      volumes = [
        "cardano-ipc:/ipc"
      ];
      restart = "on-failure";
      depends_on = [ "wallet" "pab" ];
      command = [
        "${pkgs.bash}/bin/bash"
        "-c"
        ''
          set -euo pipefail
          exec ${marlowe-cli}/bin/marlowe-cli test contracts \
            --testnet-magic 1564 \
            --socket-path /ipc/node.socket \
            --wallet-url http://wallet:8090 \
            --pab-url http://pab:9080 \
            --faucet-key ${../payment.skey} \
            --faucet-address addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
            --burn-address addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7 \
            --passphrase fixme-allow-pass-per-wallet \
            ${../marlowe-cli/test/test-zcb.yaml} 2>&1
        ''
      ];
    };
  };

in
{
  config.services = {
    inherit node-seeder;
    inherit node;
    inherit wallet;
    inherit chain-index;
    inherit pab;
    inherit marlowe-run;
    # inherit marlowe-cli-tests;
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
