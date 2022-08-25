{ system
, packagesBySystem
, inputs
}:
let
  packages = packagesBySystem.${system};
  inherit (packages) pkgs marlowe;
  inherit (marlowe) haskell;
  inherit (haskell.packages.cardano-wallet.components.exes) cardano-wallet;
  inherit (haskell.packages.plutus-chain-index.components.exes) plutus-chain-index;
  inherit (haskell.packages.marlowe-dashboard-server.components.exes) marlowe-dashboard-server;
in
rec {
  inherit pkgs marlowe cardano-wallet plutus-chain-index;

  inherit (marlowe) webCommon web-ghc cardano-node cardano-cli;

  inherit (haskell.packages.marlowe.components.exes) marlowe-pab;

  inherit (haskell.packages.marlowe-cli.components.exes) marlowe-cli;

  webCommonMarlowe = pkgs.callPackage ./web-common-marlowe { inherit (marlowe.lib) gitignore-nix; };

  marlowe-playground = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-playground-client {
      inherit (marlowe.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonMarlowe;
      inherit (marlowe) purs-tidy writeShellScriptBinInRepoRoot;
      inherit (pkgs.nodePackages) prettier;
    }) client server generated-purescript generate-purescript start-backend build-client;
  };

  marlowe-dashboard = pkgs.recurseIntoAttrs rec {
    pkg = pkgs.callPackage ./marlowe-dashboard-client {
      inherit haskell;
      inherit (marlowe.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonMarlowe;
      inherit (marlowe) purs-tidy writeShellScriptBinInRepoRoot;
      inherit (pkgs.nodePackages) prettier;
    };
    inherit (pkg)
      client marlowe-invoker marlowe-run-backend-invoker generated-purescript
      generate-purescript start-backend build-client test-client;
  };


  dev-scripts = import ./nix/dev/scripts.nix {
    inherit pkgs packagesBySystem marlowe;
    inherit cardano-cli marlowe-pab cardano-node plutus-chain-index;
    network = pkgs.networks.testnet-dev;
    marlowe-dashboard = marlowe-dashboard.marlowe-run-backend-invoker;

  };

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs inputs;
    inherit (marlowe.lib) gitignore-nix;
    inherit (marlowe) fixStylishHaskell fix-purs-tidy fix-prettier fixPngOptimization;
    inherit (haskell) plutus-pab;
    inherit marlowe-playground marlowe-dashboard web-ghc marlowe-pab;
    src = ./.;
    run-generated = marlowe-dashboard.generated-purescript;
    play-generated = marlowe-playground.generated-purescript;
  };

  docs = import ./nix/docs.nix { inherit pkgs marlowe; };

  # Test data needed by marlowe-actus
  inherit (inputs) actus-tests;

  # Packages needed for the bitte deployment
  entrypoints = import ./bitte {
    inherit marlowe-playground web-ghc marlowe-pab marlowe-dashboard docs pkgs inputs cardano-wallet plutus-chain-index marlowe-dashboard-server;
    inherit (marlowe) cardano-node;
  };
}
