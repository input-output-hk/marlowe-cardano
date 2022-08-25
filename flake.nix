{
  description = "Marlowe Cardano implementation";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.follows = "haskell-nix/nixpkgs-2205";

    haskell-nix.url = "github:input-output-hk/haskell.nix";

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
    haskell-language-server = {
      # Pinned to a release
      url = "github:haskell/haskell-language-server?ref=1.3.0";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix/marlowe-dev-testnet";
      flake = false;
    };
    npmlock2nix = {
      url = "github:tweag/npmlock2nix";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
      flake = false;
    };
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps?rev=682977c8c9fe181a0dc066ac2b40a4b1c1b5072c";
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
    web-common = {
      url = "github:input-output-hk/purescript-web-common";
      flake = true;
    };
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    in
    (flake-utils.lib.eachSystem systems (system:
      let
        packages = self.internal.packagesFun { inherit system; };
        packagesProf = self.internal.packagesFun { inherit system; enableHaskellProfiling = true; };
      in
      {
        inherit packages;

        apps = {
          updateMaterialized = {
            type = "app";
            program =
              "${packages.dev-scripts.updateMaterialized}/bin/updateMaterialized";
          };

          update-client-deps = {
            type = "app";
            program =
              "${packages.marlowe.updateClientDeps}/bin/update-client-deps";
          };

          nixpkgs-fmt = {
            type = "app";
            program =
              "${packages.pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
          };
        };

        devShells.default = import ./dev-shell.nix {
          inherit system packages;
        };

        devShells.prof = import ./dev-shell.nix {
          inherit system;
          packages = packagesProf;
        };
      })) // {
      hydraJobs = import ./hydra-jobs.nix {
        inherit inputs;
        inherit (self) internal;
      };

      internal.packagesFun =
        { system
        , checkMaterialization ? false
        , enableHaskellProfiling ? false
        , source-repo-override ? { }
        , crossSystem ? null
        , evalSystem ? system
        }: import ./packages.nix {
          inherit system inputs;
          packagesBySystem = builtins.listToAttrs (map
            (system': {
              name = system';
              value = import ./nix {
                system = system';
                inherit checkMaterialization enableHaskellProfiling source-repo-override inputs crossSystem evalSystem;
                inherit (inputs) haskell-nix;
              };
            })
            systems);
        };
    };
}
