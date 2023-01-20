{
  description = "Marlowe Cardano implementation";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.follows = "haskell-nix/nixpkgs-2205";
    nixpkgs-unstable.follows = "haskell-nix/nixpkgs-unstable";

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
    dapps-world = {
      url = "github:input-output-hk/dapps-world";
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

    nosys.url = "github:divnix/nosys";
    std.url = "github:divnix/std";
    data-merge.url = "github:divnix/data-merge";
    bitte-cells.url = "github:input-output-hk/bitte-cells";
  };

  outputs = { self, flake-utils, nosys, tullia, ... }@inputs:
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
        inherit packages;

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

          chainseekd = {
            type = "app";
            program = "${packages.chainseekd}/bin/chainseekd";
          };

          marlowe-chain-indexer = {
            type = "app";
            program = "${packages.marlowe-chain-indexer}/bin/marlowe-chain-indexer";
          };

          marlowe-indexer = {
            type = "app";
            program = "${packages.marlowe-indexer}/bin/marlowe-indexer";
          };

          marlowe-sync = {
            type = "app";
            program = "${packages.marlowe-sync}/bin/marlowe-sync";
          };

          marlowe-history = {
            type = "app";
            program = "${packages.marlowe-history}/bin/marlowe-history";
          };

          marlowe-discovery = {
            type = "app";
            program = "${packages.marlowe-discovery}/bin/marlowe-discovery";
          };

          marlowe-tx = {
            type = "app";
            program = "${packages.marlowe-tx}/bin/marlowe-tx";
          };

          marlowe-web-server = {
            type = "app";
            program = "${packages.marlowe-web-server}/bin/marlowe-web-server";
          };

          marlowe = {
            type = "app";
            program = "${packages.marlowe-rt}/bin/marlowe";
          };

          marlowe-integration-tests = {
            type = "app";
            program = "${packages.marlowe-integration-tests}/bin/marlowe-integration-tests";
          };
        };

        devShells.default = import ./dev-shell.nix {
          inherit system packages;
        };

        devShells.prof = import ./dev-shell.nix {
          inherit system;
          packages = packagesProf;
        };

        devShells.ops = inputs.dapps-world.${system}.automation.devshells.ops;

        # 4 Layers of Packaging
        operables = import ./nix/operables.nix {
          inputs = nosys.lib.deSys system inputs;
        };
        oci-images = import ./nix/oci-images.nix {
          inputs = nosys.lib.deSys system inputs;
        };
        nomadTasks = import ./nix/nomadTasks.nix {
          inputs = nosys.lib.deSys system inputs;
        };

        nomadEnv =
          let
            envData = import ./nix/nomadEnv {
              inputs = nosys.lib.deSys system inputs;
            };
            mkNomadJobs =
              let
                pkgs = inputs.nixpkgs.legacyPackages.${system};
              in
              builtins.mapAttrs (
                n: job:
                  pkgs.linkFarm "job.${n}" [
                    {
                      name = "job";
                      path = pkgs.writeText "${n}.nomad.json" (builtins.toJSON job);
                    }
                  ]
              );
          in
          mkNomadJobs envData;

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
      inherit inputs;
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

  nixConfig = {
    extra-substituters = [
      # TODO: spongix
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    # post-build-hook = "./upload-to-cache.sh";
    allow-import-from-derivation = "true";
  };

}
