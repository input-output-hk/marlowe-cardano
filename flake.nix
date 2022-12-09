{
  description = "Marlowe Cardano implementation";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.follows = "haskell-nix/nixpkgs-2205";

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    actus-tests = {
      url = "github:actusfrf/actus-tests";
      flake = false;
    };
    haskell-language-server = {
      # Pinned to a release
      url = "github:haskell/haskell-language-server?ref=1.3.0";
      flake = false;
    };
    npmlock2nix = {
      url = "github:tweag/npmlock2nix";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix/marlowe-dev-testnet";
      flake = false;
    };
    cardano-world = {
      url = "github:input-output-hk/cardano-world";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    plutus-core = {
      url = "github:input-output-hk/plutus";
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
    tullia = {
      url = "github:input-output-hk/tullia";
    };
  };

  outputs = { self, flake-utils, tullia, ... }@inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    in
    (flake-utils.lib.eachSystem systems (system:
      let
        packages = self.internal.packagesFun { inherit system; };
        packagesLinux = self.internal.packagesFun { system = "x86_64-linux"; };
        packagesProf = self.internal.packagesFun { inherit system; enableHaskellProfiling = true; };
      in
      {
        packages = packages // {
          required = self.ciJobs.${system}.required;
        };

        apps = rec {
          updateMaterialized = {
            type = "app";
            program =
              "${packages.dev-scripts.updateMaterialized}/bin/updateMaterialized";
          };

          nixpkgs-fmt = {
            type = "app";
            program =
              "${packages.pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
          };

          refresh-compose = {
            type = "app";
            program = (packages.pkgs.writeShellScript "refresh-compose" ''
              cd $(git rev-parse --show-toplevel)

              nix-store --realise ${packagesLinux.compose-spec} --add-root compose.yaml --indirect
            '').outPath;
          };

          re-up = {
            type = "app";
            program = (packages.pkgs.writeShellScript "re-up" ''
              cd $(git rev-parse --show-toplevel)

              ${refresh-compose.program}
              docker compose up --detach
            '').outPath;
          };
        };

        devShells.default = import ./dev-shell.nix {
          inherit system packages;
        };

        devShells.prof = import ./dev-shell.nix {
          inherit system;
          packages = packagesProf;
        };

        # Export ciJobs for tullia to parse
        ciJobs = self.hydraJobs {
          supportedSystems = [ system ];
        };
      }
      // tullia.fromSimple system (import ./nix/tullia.nix)
    )) // {
      hydraJobs = import ./hydra-jobs.nix {
        inherit inputs;
        inherit (self) internal;
        marlowe-cardano = self;
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
