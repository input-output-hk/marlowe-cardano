{ pkgs
, network
, cardano-cli
, cardano-node
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
  };

  run-chainseekd = writeShellScriptBinInRepoRoot "run-chainseekd" ''
    cd marlowe-chain-sync
    cabal run chainseekd -- \
      --testnet-magic ${builtins.toString network.magic} \
      --socket-path ${devNetworkConfig.node.socket-path} \
      --database-uri postgresql://postgres@0.0.0.0/chain \
      --genesis-config-file ${network.nodeConfig.ByronGenesisFile} \
      --genesis-config-file-hash ${network.nodeConfig.ByronGenesisHash}
  '';

  start-cardano-node = writeShellScriptBinInRepoRoot "start-cardano-node" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"
    export DATA_DIR=${devNetworkConfig.node.database-path}
    export ENVIRONMENT=${network.name}
    export SOCKET_POATH=${devNetworkConfig.node.socket-path}
    mkdir -p ${devNetworkConfig.node.database-path}
    ${pkgs.cardano.entrypoints.cardano-node}/bin/entrypoint
  '';

  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';

  nix-flakes-alias = pkgs.runCommand "nix-flakes-alias" { } ''
    mkdir -p $out/bin
    ln -sv ${pkgs.nixFlakes}/bin/nix $out/bin/nix-flakes
  '';

in
{
  inherit nix-flakes-alias run-chainseekd start-cardano-node;
}
