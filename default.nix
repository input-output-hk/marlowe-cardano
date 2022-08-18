########################################################################
# default.nix -- The top-level nix build file for Marlowe.
#
# This file defines various attributes that are used for building and
# developing Marlowe.
#
########################################################################
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, haskellNix ? import sources.haskell-nix {
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix checkMaterialization enableHaskellProfiling source-repo-override; }
  # An explicit git rev to use, passed when we are in Hydra
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
, source-repo-override ? { }
, networkNixName ? "preview"
}:
let
  inherit (packages) pkgs marlowe sources;
  inherit (marlowe) haskell;
in
rec {
  inherit pkgs marlowe;

  inherit (marlowe) cardano-node cardano-cli;

  inherit (haskell.packages.marlowe-cli.components.exes) marlowe-cli;

  network = pkgs.networks.${networkNixName};

  dev-scripts = import ./nix/dev/scripts.nix {
    inherit pkgs network cardano-cli cardano-node;
  };

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs sources;
    inherit (marlowe.lib) gitignore-nix;
    inherit (marlowe) fixStylishHaskell fix-prettier;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs marlowe; };

  # Test data needed by marlowe-actus provided via niv
  inherit (sources) actus-tests;

  # Packages needed for the bitte deployment
  bitte-packages = import ./bitte {
    inherit cardano-node docs pkgs sources;
  };
}
