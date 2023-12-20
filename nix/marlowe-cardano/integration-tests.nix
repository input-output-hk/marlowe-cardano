{ repoRoot, inputs, pkgs, lib, system }:

let
  cabalProject = inputs.self.cabalProject;

  cardano-cli = repoRoot.nix.marlowe-cardano.cardano-tools.cardano-cli;
  cardano-node = repoRoot.nix.marlowe-cardano.cardano-tools.cardano-node;
  marlowe-minting-validator = inputs.marlowe-plutus.packages.marlowe-minting-validator;

  marlowe-runtime-cli = cabalProject.hsPkgs.marlowe-runtime-cli.components.exes.marlowe-runtime-cli;
  marlowe-integration-tests = cabalProject.hsPkgs.marlowe-integration-tests.components.exes.marlowe-integration-tests;

  runtimeInputs = [
    cardano-cli
    cardano-node
    marlowe-minting-validator
    marlowe-runtime-cli
    pkgs.z3
    pkgs.sqitchPg
    pkgs.postgresql
  ];

in
pkgs.writeScriptBin "marlowe-integration-tests" ''
  export PATH="${lib.makeBinPath runtimeInputs}:$PATH"
  export PGUSER=postgres
  ${marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
''
