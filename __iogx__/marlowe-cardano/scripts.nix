{ inputs, pkgs, haskell-nix-project }:

let
  networkNixName = "preprod";

  # networks = import ./networks.nix { inherit inputs pkgs; };
  network = inputs.self.networks.__iogx__.${networkNixName}; # TODO remove the __iogx__ when merged
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
    ${inputs.self.packages.__iogx__.entrypoints.cardano-node}/bin/cardano-node 
  ''; # TODO remove the __iogx__ when merged

  re-up = ''
    cd $(git rev-parse --show-toplevel)

    docker compose up --detach --file ${compose-spec} 
  '';

  compose-spec = import ./compose-spec.nix { inherit inputs pkgs; };

  mkCabalExeScript = target: ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    cabal build ${target} 1>/dev/null 2>/dev/null
    cabal run ${target} -- "$@" | tail -n +2
  '';

  marlowe-runtime-cli = mkCabalExeScript "marlowe-runtime-cli";

  marlowe-cli = mkCabalExeScript "marlowe-cli";

  run-integration-tests =
    let
      cardano-cli = inputs.cardano-world.cardano.packages.cardano-cli;
      cardano-node = inputs.cardano-world.cardano.packages.cardano-node;
      marlowe-runtime-cli = haskell-nix-project.hsPkgs.marlowe-runtime-cli.components.exes.marlowe-runtime-cli;
      marlowe-integration-tests = haskell-nix-project.hsPkgs.marlowe-integration-tests.components.exes.marlowe-integration-tests;
    in
    ''
      export PATH="${pkgs.lib.makeBinPath [ cardano-cli cardano-node marlowe-runtime-cli pkgs.sqitchPg pkgs.postgresql ]}:$PATH"
      export PGUSER=postgres
      ${marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
    '';
in
{
  inherit
    start-cardano-node
    re-up
    compose-spec
    run-integration-tests
    marlowe-runtime-cli
    marlowe-cli;
}
