{ pkgs
, network
, marlowe-dashboard
, cardano-cli
, marlowe-pab
, cardano-node
, plutus-chain-index
}:
let
  network = pkgs.networks.testnet-dev;
  marlowe-pab-exe = marlowe-pab + "/bin/marlowe-pab";
  marlowe-dashboard-exe = marlowe-dashboard + "/bin/marlowe-dashboard-server";

  devNetworkConfig = rec {
    node = {
      config-file = pkgs.writeTextFile {
        name = "node-config.json";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/node-config.nix { config = network.nodeConfig; });
      };
      port = 3001;
      socket-path = "/tmp/node.socket";
      database-path = "db/node.db";
    };
    wallet = {
      testnet = network.nodeConfig.ByronGenesisFile;
      database-path = "db/wallet.db";
      port = 8090;
    };
    chain-index = {
      database-path = "db/chain-index.db";
      port = 9083;
    };
    pab = {
      database-path = "db/pab.db";
      port = 9080;
      config-params = {
        dbConfigFile = pab.database-path + "/marlowe-pab.db";
        baseUrl = "http://localhost:${toString pab.port}";
        walletUrl = "http://localhost:${toString wallet.port}";
        chainIndexUrl = "http://localhost:${toString chain-index.port}";
        socket-path = node.socket-path;
        inherit network;
        protocol-parameters = "/tmp/testnet.protocol";
      };
      config-file = pkgs.writeTextFile {
        name = "marlowe-pab.yaml";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/pab-config.nix pab.config-params);
      };
    };

    dashboard-server = {
      config-params = {
        chain-index-port = chain-index.port;
        chain-index-host = "localhost";
        wallet-port = wallet.port;
        wallet-host = "localhost";
      };
      config-file = pkgs.writeTextFile {
        name = "dashboard-server.json";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/marlowe-run.nix dashboard-server.config-params);
      };
      port = 8080;
    };
    topology = network.topology;
  };

  start-cardano-node = writeShellScriptBinInRepoRoot "start-cardano-node" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"
    mkdir -p ${devNetworkConfig.node.database-path}
    cardano-node run \
            --config ${devNetworkConfig.node.config-file} \
            --topology ${devNetworkConfig.topology} \
            --port ${toString devNetworkConfig.node.port} \
            --socket-path ${devNetworkConfig.node.socket-path} \
            --database-path ${devNetworkConfig.node.database-path}
  '';

  start-wallet = writeShellScriptBinInRepoRoot "start-cardano-wallet" ''
    mkdir -p ${devNetworkConfig.wallet.database-path}

    cardano-wallet serve \
      --testnet ${devNetworkConfig.wallet.testnet} \
      --database ${devNetworkConfig.wallet.database-path} \
      --node-socket ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.wallet.port}
  '';

  start-chain-index = writeShellScriptBinInRepoRoot "start-chain-index" ''
    mkdir -p ${devNetworkConfig.chain-index.database-path}

    ${plutus-chain-index}/bin/plutus-chain-index start-index \
      --network-id ${toString network.magic} \
      --db-path ${devNetworkConfig.chain-index.database-path}/ci.sqlite \
      --socket-path ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.chain-index.port}

  '';

  start-marlowe-pab = writeShellScriptBinInRepoRoot "start-marlowe-pab" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"
    mkdir -p ${devNetworkConfig.pab.database-path}

    [ ! -f ${devNetworkConfig.pab.config-params.dbConfigFile}/marlowe-pab.db ] && \
      ${marlowe-pab-exe} migrate --config ${devNetworkConfig.pab.config-file}

    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    CARDANO_NODE_SOCKET_PATH=${devNetworkConfig.node.socket-path} ${cardano-cli}/bin/cardano-cli query \
            protocol-parameters \
            --testnet-magic ${toString network.magic} \
            --out-file ${devNetworkConfig.pab.config-params.protocol-parameters}

    ${marlowe-pab-exe} webserver \
            --config ${devNetworkConfig.pab.config-file} \
            --passphrase fixme-allow-pass-per-wallet \
            --memory
  '';

  start-dashboard-server = writeShellScriptBinInRepoRoot "start-dashboard-server" ''
    ${marlowe-dashboard-exe} webserver \
      --config ${devNetworkConfig.dashboard-server.config-file} \
      --port ${toString devNetworkConfig.dashboard-server.port} \
      --network-id ${toString network.magic} \
      --verbosity 2
  '';

  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';


in
{
  inherit start-cardano-node start-wallet start-chain-index start-marlowe-pab start-dashboard-server;
}
