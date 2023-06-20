{ inputs, pkgs, project }:

let
  networkNixName = "preprod";

  network = inputs.self.networks.${networkNixName};

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
  '';

  re-up = ''
    cd $(git rev-parse --show-toplevel)

    docker compose -f ${compose-spec} up -d
  '';

  compose-spec = import ./compose.nix { inherit inputs pkgs; };

  gen-compose-spec = ''
    cat ${compose-spec} 
  '';

  mkCabalExeScript = target: ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    cabal build ${target} 1>/dev/null 2>/dev/null
    cabal run ${target} -- "$@" | tail -n +2
  '';

  marlowe-runtime-cli = mkCabalExeScript "marlowe-runtime-cli";

  marlowe-cli = mkCabalExeScript "marlowe-cli";
in
{
  inherit
    start-cardano-node
    re-up
    gen-compose-spec
    marlowe-runtime-cli
    marlowe-cli;
}
