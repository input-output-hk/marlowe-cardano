{ system
, packagesBySystem
, inputs
, networkNixName ? "preprod"
}:
let
  packages = packagesBySystem.${system};
  inherit (packages) pkgs marlowe;
  inherit (marlowe) haskell;
in
rec {
  inherit pkgs marlowe;

  # TODO This file is a mess, why does our flake expose cardano-node and
  # cardano-cli as packages? We need to rethink what we export from our flake
  # and make it a bit more reasonable.
  inherit (pkgs.cardano.packages) cardano-node cardano-cli;

  inherit (haskell.packages.marlowe-cli.components.exes) marlowe-cli;
  inherit (haskell.packages.marlowe-chain-sync.components.exes) marlowe-chain-sync marlowe-chain-indexer;
  inherit (haskell.packages.marlowe-runtime.components.exes) marlowe-sync marlowe-indexer marlowe-tx marlowe-proxy;
  inherit (haskell.packages.marlowe-runtime-web.components.exes) marlowe-web-server;
  inherit (haskell.packages.marlowe-runtime-cli.components.exes) marlowe-runtime-cli;
  marlowe-integration-tests = pkgs.writeShellScriptBin "marlowe-integration-tests" ''
    export PATH="${pkgs.lib.makeBinPath [ cardano-cli cardano-node pkgs.sqitchPg marlowe-runtime-cli ]}:$PATH"
    ${haskell.packages.marlowe-integration-tests.components.exes.marlowe-integration-tests}/bin/marlowe-integration-tests "$@"
  '';

  network = pkgs.networks.${networkNixName};

  compose-spec = pkgs.callPackage ./nix/dev/compose.nix { };

  dev-scripts = import ./nix/dev/scripts.nix {
    inherit pkgs packagesBySystem marlowe network cardano-cli cardano-node;
  };

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (marlowe) fixStylishHaskell fix-prettier;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs marlowe; };

  # Packages needed for the bitte deployment
  entrypoints = import ./bitte {
    inherit cardano-node pkgs;
  };
}
