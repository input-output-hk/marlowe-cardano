{ system
, packagesBySystem
, inputs
, networkNixName ? "preview"
}:
let
  packages = packagesBySystem.${system};
  inherit (packages) pkgs marlowe;
  inherit (marlowe) haskell;
in
rec {
  inherit pkgs marlowe;

  inherit (marlowe) cardano-node cardano-cli;

  inherit (haskell.packages.marlowe-cli.components.exes) marlowe-cli;

  network = pkgs.networks.${networkNixName};

  dev-scripts = import ./nix/dev/scripts.nix {
    inherit pkgs packagesBySystem marlowe network cardano-cli cardano-node;
  };

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (marlowe) fixStylishHaskell fix-prettier;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs marlowe; };

  # Test data needed by marlowe-actus
  inherit (inputs) actus-tests;

  # Packages needed for the bitte deployment
  entrypoints = import ./bitte {
    inherit cardano-node pkgs;
  };
}
