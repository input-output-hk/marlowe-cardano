# NOTE: This flake is only provided as interface to `bitte` and shouldn't be used otherwise
#
# Occasionally building flake builds will segfault. The workaround for this is to
# disable the garbage collector  `GC_DONT_GC=1  nix build .#web-ghc-server
#
# In case you are not sure if you should be using this flake, the answer is: No.
{
  description = "plutus flake for pinning sources and bitte deployments";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    # We intentionally import nixpkgs and haskell.nix as non-flakes, to match the
    # flake-free normal build workflow exactly.
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };

    haskellNix = {
      # TODO: fix ref usage to master
      url = "github:input-output-hk/haskell.nix?rev=df363a0b30abbeb8b40a9223fe8ec224b4d6d358";
    };

    actus-tests = {
      url = "github:actusfrf/actus-tests";
      flake = false;
    };
    cardano-repo-tool = {
      url = "github:input-output-hk/cardano-repo-tool";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    gitignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskell-language-server = {
      # Pinned to a release
      url = "github:haskell/haskell-language-server?ref=1.3.0";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };
    npmlock2nix = {
      url = "github:tweag/npmlock2nix";
      flake = false;
    };
    ops-lib.url = "github:input-output-hk/ops-lib";
    plutus-core = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps";
      flake = false;
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      flake = false;
    };
    sphinxcontrib-haddock = {
      url = "github:michaelpj/sphinxcontrib-haddock";
      flake = false;
    };
    stackage-nix = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    web-common = {
      url = "github:input-output-hk/purescript-web-common";
      flake = true;
    };
  };

  outputs = { self, flake-utils, nixpkgs, haskellNix, hackage-nix, stackage-nix, ops-lib, ... }@inputs:
      (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (import ./nix/overlays/marlowe.nix)
          (import ./nix/overlays/nixpkgs-overrides.nix)
          (import ./nix/overlays/r.nix)
        ];

        pkgsForSystem = system: import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };

        pkgs = pkgsForSystem system;
        hydraUtils = ops-lib.lib.mkHydraUtils pkgsForSystem;
        inherit (hydraUtils) collectHydraSets mkHydraSet;
        flake = pkgs.marloweProject.flake { };
      in flake // {
        inherit pkgs;
      } // (collectHydraSets [
        #(mkHydraSet [ "marlowe" ] [ "x86_64-linux" ])
      ])));
}
