{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
    let
      mkIfDarwin = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin;
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      name = "marlowe-cardano";

      src = ../.;

      compiler-nix-name = "ghc928";

      flake.variants.profiled.modules = [{
        enableProfiling = true;
        enableLibraryProfiling = true;
      }];

      shell.withHoogle = false;

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
      };

      modules = [{

        # This reduces the shell size by 600MB
        dontStrip = false;

        packages = {
          # async-components
          # cardano-integration
          # eventuo11y-extras
          # hasql-dynamic-syntax
          # marlowe
          # marlowe-actus
          # marlowe-apps
          # marlowe-chain-sync
          # marlowe-cli
          # marlowe-client
          # marlowe-contracts
          # marlowe-integration
          # marlowe-integration-tests
          # marlowe-object
          # marlowe-protocols
          # marlowe-runtime
          # marlowe-runtime-cli
          # marlowe-runtime-web
          # marlowe-test

          # Fix missing executables on the paths of the test runners. This is arguably
          # a bug, and the fix is a bit of a hack.
          marlowe.components.tests.marlowe-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';

          marlowe-contracts.components.tests.marlowe-contracts-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';

          marlowe-test.components.tests.marlowe-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';

          marlowe-runtime.components.tests.marlowe-runtime-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';

          marlowe-cli.components.tests.marlowe-cli-test.preCheck = ''
            PATH=${lib.makeBinPath [ pkgs.z3 ]}:$PATH
          '';

          # Note: The following two statements say that these tests should
          # _only_ run on linux. In actual fact we just don't want them
          # running on the 'mac-mini' instances, because these tests time out
          # there. In an ideal world this would be reflected here more
          # accurately.
          # TODO: Resolve this situation in a better way.
          marlowe.components.tests.marlowe-test-long-running = {
            platforms = lib.platforms.linux;
          };

          # These libraries rely on a TemplateHaskell splice that requires
          # git to be in the path at build time. This only seems to affect
          # Darwin builds, and including them on Linux breaks lorri, so we
          # only add these options when building on Darwin.
          marlowe-cli.components.exes.marlowe-cli.build-tools =
            mkIfDarwin [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];

          async-components.ghcOptions = [ "-Werror" ];
          cardano-integration.ghcOptions = [ "-Werror" ];
          eventuo11y-extras.ghcOptions = [ "-Werror" ];
          marlowe.ghcOptions = [ "-Werror" ];
          marlowe-actus.ghcOptions = [ "-Werror" ];
          marlowe-contracts.ghcOptions = [ "-Werror" ];
          marlowe-cli.ghcOptions = [ "-Werror" ];
          marlowe-apps.ghcOptions = [ "-Werror" ];
          marlowe-chain-sync.ghcOptions = [ "-Werror" ];
          marlowe-client.ghcOptions = [ "-Werror" ];
          marlowe-integration.ghcOptions = [ "-Werror" ];
          marlowe-integration-tests.ghcOptions = [ "-Werror" ];
          marlowe-protocols.ghcOptions = [ "-Werror" ];
          marlowe-runtime.ghcOptions = [ "-Werror" ];
          marlowe-runtime-cli.ghcOptions = [ "-Werror" ];
          marlowe-runtime-web.ghcOptions = [ "-Werror" ];
          marlowe-test.ghcOptions = [ "-Werror" ];
        };
      }];
    });


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
    includeMingwW64HydraJobs = true;
  };

in

project
