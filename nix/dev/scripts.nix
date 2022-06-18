{ pkgs
, network
, marlowe-dashboard
, cardano-cli
, marlowe-pab
, cardano-node
, plutus-chain-index
}:
let
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
      port = 9080;
      config-params = {
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
    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    mkdir -p ${devNetworkConfig.wallet.database-path}

    cardano-wallet serve \
      --testnet ${devNetworkConfig.wallet.testnet} \
      --database ${devNetworkConfig.wallet.database-path} \
      --node-socket ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.wallet.port}
  '';

  start-chain-index = writeShellScriptBinInRepoRoot "start-chain-index" ''
    mkdir -p ${devNetworkConfig.chain-index.database-path}

    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    ${plutus-chain-index}/bin/plutus-chain-index start-index \
      --network-id ${toString network.magic} \
      --db-path ${devNetworkConfig.chain-index.database-path}/ci.sqlite \
      --socket-path ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.chain-index.port}

  '';

  start-marlowe-pab = writeShellScriptBinInRepoRoot "start-marlowe-pab" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"

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
    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"


    ${marlowe-dashboard-exe} webserver \
      --config ${devNetworkConfig.dashboard-server.config-file} \
      --port ${toString devNetworkConfig.dashboard-server.port} \
      --network-id ${toString network.magic} \
      --verbosity 2
  '';

  start-marlowe-run = writeShellScriptBinInRepoRoot "start-marlowe-run" ''
    #!/bin/bash

    # The spago dependencies might fail downloading, so we invoke it until it works
    cd marlowe-dashboard-client
    counter=5
    spago install
    while [[ "$?" -ne 0 && "$counter" -gt 0 ]]; do
      let "counter-=1";
      echo "Failed, retrying $counter more times";
      spago install
    done
    cd ..

    ${pkgs.tmux}/bin/tmux -T 256,mouse,focus,title\
      new-session "printf '\033]2;Cardano node\033\\' && start-cardano-node" \; \
      set mouse on \; \
      set pane-border-status top \; \
      set pane-border-format "#{pane_index} #T" \; \
      setw remain-on-exit on \; \
      split-window -h "printf '\033]2;PAB\033\\' && start-marlowe-pab" \; \
      split-window -h "printf '\033]2;WBE\033\\' && start-cardano-wallet" \; \
      split-window "printf '\033]2;Chain IX\033\\' && start-chain-index" \; \
      split-window "printf '\033]2;MRun BE\033\\' && start-dashboard-server" \; \
      split-window "printf '\033]2;MRun FE\033\\' && cd marlowe-dashboard-client && npm run start" \; \
      rename-window "Marlowe Run" \;
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
  inherit start-cardano-node start-wallet start-chain-index start-marlowe-pab start-dashboard-server start-marlowe-run;
}
