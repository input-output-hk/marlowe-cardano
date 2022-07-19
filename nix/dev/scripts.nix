{ pkgs
, network
, marlowe-dashboard
, cardano-cli
, marlowe-pab
, cardano-node
, plutus-chain-index
}:
let
  # FIXME: This was copied in anger from marlowe-dashboard-client/dev - cleanup this stuff when needed.
  nodeConfig = { AlonzoGenesisFile ? config.AlonzoGenesisFile, ByronGenesisFile ? config.ByronGenesisFile, ShelleyGenesisFile ? config.ShelleyGenesisFile, config }:
    config // {
      inherit AlonzoGenesisFile ByronGenesisFile ShelleyGenesisFile;
      TraceDNSResolver = false;
      TraceDNSSubscription = false;
      TraceIpSubscription = false;
      TraceMempool = true;
      TracePeerSelection = false;
      TracePeerSelectionActions = false;
      TracePublicRootPeers = false;
      defaultScribes = [
        [ "StdoutSK" "stdout" ]
      ];
      minSeverity = "Info";
      setupScribes = [
        {
          scFormat = "ScText";
          scKind = "StdoutSK";
          scName = "stdout";
          scRotation = null;
        }
      ];
    };

  devNetworkConfig = rec {
    topology = network.topology;
    node = {
      config-file = pkgs.writeTextFile {
        name = "node-config.json";
        text = builtins.toJSON (nodeConfig { config = network.nodeConfig; });

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

  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';


in
{
  inherit start-cardano-node start-wallet;
}
