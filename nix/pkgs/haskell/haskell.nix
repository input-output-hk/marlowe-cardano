############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, rPackages
, haskell-nix
, z3
, R
, libsodium-vrf
, secp256k1
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
, actus-tests
, source-repo-override
, evalSystem
, CHaP
}:
let
  r-packages = with rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];
  project = haskell-nix.cabalProject' ({ pkgs, ... }: {
    inherit compiler-nix-name evalSystem;
    src = ../../../.;
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/input-output-hk/nix-tools/issues/97
    materialized =
      if source-repo-override != { } then null
      else if pkgs.stdenv.hostPlatform.isLinux then ./materialized-linux
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization source-repo-override;
    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need plutus-tx-plugin
          marlowe.package.buildable = false; # Would also require libpq
          marlowe-actus.package.buildable = false;
          marlowe-contracts.package.buildable = false;
          marlowe-cli.package.buildable = false;
          plutus-ledger.package.buildable = false;
          plutus-tx-plugin.package.buildable = false;
        };
      })
      ({ pkgs, config, ... }: {
        packages =
          let
            mkIfDarwin = lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin);
          in
          {
            # These libraries rely on a TemplateHaskell splice that requires
            # git to be in the path at build time. This only seems to affect
            # Darwin builds, and including them on Linux breaks lorri, so we
            # only add these options when building on Darwin.
            cardano-config.components.library.build-tools = mkIfDarwin [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];
            marlowe-cli.components.exes. marlowe-cli.build-tools = mkIfDarwin [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];

            # See https://github.com/input-output-hk/plutus/issues/1213 and
            # https://github.com/input-output-hk/plutus/pull/2865.
            marlowe.doHaddock = deferPluginErrors;
            marlowe.flags.defer-plugin-errors = deferPluginErrors;

            plutus-contract.doHaddock = deferPluginErrors;
            plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

            plutus-use-cases.doHaddock = deferPluginErrors;
            plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

            plutus-ledger.doHaddock = deferPluginErrors;
            plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

            # Packages we just don't want docs for
            plutus-benchmark.doHaddock = false;
            # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
            plutus-tx-plugin.doHaddock = false;
            plutus-script-utils.doHaddock = false;

            # Fix missing executables on the paths of the test runners. This is arguably
            # a bug, and the fix is a bit of a hack.
            marlowe.components.tests.marlowe-test.preCheck = ''
              PATH=${lib.makeBinPath [ z3 ]}:$PATH
            '';

            marlowe-contracts.components.tests.marlowe-contracts-test.preCheck = ''
              PATH=${lib.makeBinPath [ z3 ]}:$PATH
            '';

            marlowe-test.components.tests.marlowe-test.preCheck = ''
              PATH=${lib.makeBinPath [ z3 ]}:$PATH
            '';

            # Relies on cabal-doctest, just turn it off in the Nix build
            prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

            plutus-core.components.benchmarks.update-cost-model = {
              build-tools = r-packages;
              # Seems to be broken on darwin for some reason
              platforms = lib.platforms.linux;
            };

            plutus-core.components.benchmarks.cost-model-test = {
              build-tools = r-packages;
              # Seems to be broken on darwin for some reason
              platforms = lib.platforms.linux;
            };

            marlowe-actus.components.exes.marlowe-shiny = {
              build-tools = r-packages;
              # Seems to be broken on darwin for some reason
              platforms = lib.platforms.linux;
            };

            # The marlowe-actus tests depend on external data which is
            # provided from Nix (as niv dependency)
            marlowe-actus.components.tests.actus-core-test.preCheck = ''
              export ACTUS_TEST_DATA_DIR=${actus-tests}/tests/
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

            # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
            iohk-monitoring.doHaddock = false;

            # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
            async-components.ghcOptions = [ "-Werror" ];
            cardano-integration.ghcOptions = [ "-Werror" ];
            eventuo11y-extras.ghcOptions = [ "-Werror" ];
            marlowe.ghcOptions = [ "-Werror" ];
            marlowe-actus.ghcOptions = [ "-Werror" ];
            marlowe-chain-sync.ghcOptions = [ "-Werror" ];
            marlowe-cli.ghcOptions = [ "-Werror" ];
            marlowe-contracts.ghcOptions = [ "-Werror" ];
            marlowe-integration.ghcOptions = [ "-Werror" ];
            marlowe-integration-tests.ghcOptions = [ "-Werror" ];
            marlowe-protocols.ghcOptions = [ "-Werror" ];
            marlowe-protocols-test.ghcOptions = [ "-Werror" ];
            marlowe-runtime.ghcOptions = [ "-Werror" ];
            marlowe-test.ghcOptions = [ "-Werror" ];

            # External package settings

            inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

            # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
            # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
            ieee.components.library.libs = lib.mkForce [ ];

            # See https://github.com/input-output-hk/iohk-nix/pull/488
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf secp256k1 ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf secp256k1 ] ];

            # hpack fails due to modified cabal file, can remove when we bump to 3.12.0
            cardano-addresses.cabal-generator = lib.mkForce null;
            cardano-addresses-cli.cabal-generator = lib.mkForce null;
          };
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableProfiling = true;
    };
  });

in
project
