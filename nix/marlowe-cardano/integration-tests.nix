{ inputs', pkgs, l, ... }:

{ projects }:

let
  cardano-cli = inputs'.cardano-world.cardano.packages.cardano-cli;
  cardano-node = inputs'.cardano-world.cardano.packages.cardano-node;
  marlowe-runtime-cli = projects.default.hsPkgs.marlowe-runtime-cli.components.exes.marlowe-runtime-cli;
  marlowe-integration-tests = projects.default.hsPkgs.marlowe-integration-tests.components.exes.marlowe-integration-tests;
in
pkgs.writeScriptBin "marlowe-integration-tests" ''
  export PATH="${l.makeBinPath [ cardano-cli cardano-node marlowe-runtime-cli pkgs.z3 pkgs.sqitchPg pkgs.postgresql ]}:$PATH"
  export PGUSER=postgres
  ${marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
''
