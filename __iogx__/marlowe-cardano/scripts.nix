{ inputs, pkgs }:

let
  networkNixName = "preprod";

  # networks = import ./networks.nix { inherit inputs pkgs; };
  network = inputs.self.networks.${networkNixName}; # TODO remove the __iogx__ when merged
  # network = networks.${networkNixName};

  inherit (pkgs) lib;

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

  start-cardano-node = ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"
    export DATA_DIR=${devNetworkConfig.node.database-path}
    export ENVIRONMENT=${network.name}
    export SOCKET_PATH=${devNetworkConfig.node.socket-path}
    mkdir -p ${devNetworkConfig.node.database-path}
    ${inputs.self.packages.entrypoints.cardano-node}/bin/cardano-node 
  ''; # TODO remove the __iogx__ when merged

  re-up = ''
    cd $(git rev-parse --show-toplevel)

    docker compose up --detach --file ${import ./compose-spec.nix { inherit inputs pkgs; }} 
  '';

  # TODO [iogx]
  # marlowe-integration-tests = ''
  #   export PATH="${pkgs.lib.makeBinPath [ cardano-cli cardano-node pkgs.sqitchPg marlowe-rt ]}:$PATH"
  #   ${haskell.packages.marlowe-integration-tests.components.exes.marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
  # '';
  # marlowe-rt = haskell.packages.marlowe-runtime-cli.components.exes.marlowe;
  # network = pkgs.networks.${networkNixName};
in
{
  inherit start-cardano-node re-up;
}
