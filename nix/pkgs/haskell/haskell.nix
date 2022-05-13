############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, rPackages
, haskell-nix
, gitignore-nix
, z3
, R
, libsodium-vrf
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
, actus-tests
, source-repo-override
}:
let
  r-packages = with rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];
  project = haskell-nix.cabalProject' ({ pkgs, ... }: {
    inherit compiler-nix-name;
    # This is incredibly difficult to get right, almost everything goes wrong, see https://github.com/input-output-hk/haskell.nix/issues/496
    src = let root = ../../../.; in
      haskell-nix.haskellLib.cleanSourceWith {
        filter = gitignore-nix.gitignoreFilter root;
        src = root;
        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "plutus";
      };
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/input-output-hk/nix-tools/issues/97
    materialized =
      if source-repo-override != { } then null
      else if pkgs.stdenv.hostPlatform.isLinux then ./materialized-linux
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else if pkgs.stdenv.hostPlatform.isWindows then ./materialized-windows
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization source-repo-override;
    sha256map = import ./sha256map.nix;
    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling for windows we don't have a `ghc` package, so use
      -- the `plutus-ghc-stub` package instead.
      package plutus-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package marlowe
        tests: False
      package prettyprinter-configurable
        tests: False
    '';
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need plutus-tx-plugin
          marlowe.package.buildable = false; # Would also require libpq
          marlowe-actus.package.buildable = false;
          marlowe-contracts.package.buildable = false;
          marlowe-cli.package.buildable = false;
          marlowe-dashboard-server.package.buildable = false;
          marlowe-playground-server.package.buildable = false; # Would also require libpq
          marlowe-symbolic.package.buildable = false;
          playground-common.package.buildable = false;
          plutus-benchmark.package.buildable = false;
          plutus-chain-index-core.package.buildable = false;
          plutus-contract.package.buildable = false;
          plutus-errors.package.buildable = false;
          plutus-ledger.package.buildable = false;
          plutus-pab.package.buildable = false;
          plutus-playground-server.package.buildable = false; # Would also require libpq
          plutus-tx-plugin.package.buildable = false;
          plutus-use-cases.package.buildable = false;
          web-ghc.package.buildable = false;
          # These need R
          plutus-core.components.benchmarks.cost-model-test.buildable = lib.mkForce false;
          plutus-core.components.benchmarks.update-cost-model.buildable = lib.mkForce false;
        };
      })
      ({ pkgs, ... }:
        let
          # Add symlinks to the DLLs used by executable code to the `bin` directory
          # of the components with we are going to run.
          # We should try to find a way to automate this will in haskell.nix.
          symlinkDlls = ''
            ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
            ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
          '';
        in
        lib.mkIf (pkgs.stdenv.hostPlatform.isWindows) {
          packages = {
            # Add dll symlinks to the compoents we want to run.
            plutus-core.components.tests.plutus-core-test.postInstall = symlinkDlls;
            plutus-core.components.tests.plutus-ir-test.postInstall = symlinkDlls;
            plutus-core.components.tests.untyped-plutus-core-test.postInstall = symlinkDlls;
            plutus-ledger-api.components.tests.plutus-ledger-api-test.postInstall = symlinkDlls;

            # These three tests try to use `diff` and the following could be used to make the
            # linux version of diff available.  Unfortunately the paths passed to it are windows style.
            # plutus-core.components.tests.plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # plutus-core.components.tests.plutus-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # plutus-core.components.tests.untyped-plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            plutus-core.components.tests.plutus-core-test.buildable = lib.mkForce false;
            plutus-core.components.tests.plutus-ir-test.buildable = lib.mkForce false;
            plutus-core.components.tests.untyped-plutus-core-test.buildable = lib.mkForce false;
          };
        }
      )
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

            # Fix missing executables on the paths of the test runners. This is arguably
            # a bug, and the fix is a bit of a hack.
            marlowe.components.tests.marlowe-test.preCheck = ''
              PATH=${lib.makeBinPath [ z3 ]}:$PATH
            '';

            marlowe-contracts.components.tests.marlowe-contracts-test.preCheck = ''
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
            marlowe-actus.components.tests.marlowe-actus-test.preCheck = ''
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

            plutus-pab.components.tests.plutus-pab-test-full-long-running = {
              platforms = lib.platforms.linux;
            };

            # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
            iohk-monitoring.doHaddock = false;

            # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
            plutus-core.ghcOptions = [ "-Werror" ];
            marlowe.ghcOptions = [ "-Werror" ];
            marlowe-symbolic.ghcOptions = [ "-Werror" ];
            marlowe-actus.ghcOptions = [ "-Werror" ];
            marlowe-playground-server.ghcOptions = [ "-Werror" ];
            marlowe-dashboard-server.ghcOptions = [ "-Werror" ];
            marlowe-contract.ghcOptions = [ "-Werror" ];
            playground-common.ghcOptions = [ "-Werror" ];
            plutus-contract.ghcOptions = [ "-Werror" ];
            plutus-ledger.ghcOptions = [ "-Werror" ];
            plutus-ledger-api.ghcOptions = [ "-Werror" ];
            plutus-playground-server.ghcOptions = [ "-Werror" ];
            plutus-pab.ghcOptions = [ "-Werror" ];
            plutus-tx.ghcOptions = [ "-Werror" ];
            plutus-tx-plugin.ghcOptions = [ "-Werror" ];
            plutus-doc.ghcOptions = [ "-Werror" ];
            plutus-use-cases.ghcOptions = [ "-Werror" ];

            # External package settings

            inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

            # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
            # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
            ieee.components.library.libs = lib.mkForce [ ];

            # See https://github.com/input-output-hk/iohk-nix/pull/488
            cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          };
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  });

in
project
