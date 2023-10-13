{ inputs, pkgs, l, ... }:

let
  cabalProject = inputs.self.project.cabalProject;

  cardano-cli = inputs.cardano-node.packages.cardano-cli;
  cardano-node = inputs.cardano-node.packages.cardano-node;
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
  export PATH="${l.makeBinPath runtimeInputs}:$PATH"
  export PGUSER=postgres
  ${marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
''
