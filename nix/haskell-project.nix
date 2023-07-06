# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixhaskell-projectnix

{ inputs, inputs', meta, config, pkgs, lib }:

let

  lib = pkgs.lib;
  mkIfDarwin = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin;
  isNotCross = lib.mkForce (pkgs.stdenv.hostPlatform == pkgs.stdenv.buildPlatform);
  rPackages = with pkgs.rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];

  packages = {
    # Things that need plutus-tx-plugin
    plutus-ledger.package.buildable = isNotCross;
    plutus-tx-plugin.package.buildable = isNotCross;

    # These libraries rely on a TemplateHaskell splice that requires
    # git to be in the path at build time. This only seems to affect
    # Darwin builds, and including them on Linux breaks lorri, so we
    # only add these options when building on Darwin.
    cardano-config.components.library.build-tools = mkIfDarwin [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];

    plutus-contract.doHaddock = meta.enableHaddock;
    plutus-contract.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-use-cases.doHaddock = meta.enableHaddock;
    plutus-use-cases.flags.defer-plugin-errors = meta.enableHaddock;

    plutus-ledger.doHaddock = meta.enableHaddock;
    plutus-ledger.flags.defer-plugin-errors = meta.enableHaddock;

    # Packages we just don't want docs for
    plutus-benchmark.doHaddock = false;
    # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
    plutus-tx-plugin.doHaddock = false;
    plutus-script-utils.doHaddock = false;

    # Relies on cabal-doctest, just turn it off in the Nix build
    prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

    plutus-core.components.benchmarks.update-cost-model = {
      build-tools = rPackages;
      # Seems to be broken on darwin for some reason
      platforms = lib.platforms.linux;
    };

    plutus-core.components.benchmarks.cost-model-test = {
      build-tools = rPackages;
      # Seems to be broken on darwin for some reason
      platforms = lib.platforms.linux;
    };

    # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
    iohk-monitoring.doHaddock = false;

    # External package settings
    inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

    # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
    # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
    ieee.components.library.libs = lib.mkForce [ ];

    # hpack fails due to modified cabal file, can remove when we bump to 3.12.0
    cardano-addresses.cabal-generator = lib.mkForce null;
    cardano-addresses-cli.cabal-generator = lib.mkForce null;

    # Things that need plutus-tx-plugin
    marlowe.package.buildable = isNotCross; # Would also require libpq
    marlowe-actus.package.buildable = isNotCross;
    marlowe-contracts.package.buildable = isNotCross;
    marlowe-cli.package.buildable = isNotCross;

    # Things that need hs-opentelemetry-sdk (which is not available on Windows)
    async-components.package.buildable = isNotCross;
    cardano-integration.package.buildable = isNotCross;
    eventuo11y-extras.package.buildable = isNotCross;
    marlowe-apps.package.buildable = isNotCross;
    marlowe-chain-sync.package.buildable = isNotCross;
    marlowe-client.package.buildable = isNotCross;
    marlowe-integration.package.buildable = isNotCross;
    marlowe-integration-tests.package.buildable = isNotCross;
    marlowe-protocols.package.buildable = isNotCross;
    marlowe-runtime.package.buildable = isNotCross;
    marlowe-runtime-cli.package.buildable = isNotCross;
    marlowe-runtime-web.package.buildable = isNotCross;
    marlowe-test.package.buildable = isNotCross;

    # These libraries rely on a TemplateHaskell splice that requires
    # git to be in the path at build time. This only seems to affect
    # Darwin builds, and including them on Linux breaks lorri, so we
    # only add these options when building on Darwin.
    marlowe-cli.components.exes.marlowe-cli.build-tools = mkIfDarwin [ pkgs.buildPackages.buildPackages.gitReallyMinimal ];

    # See https://github.com/input-output-hk/plutus/issues/1213 and
    # https://github.com/input-output-hk/plutus/pull/2865.
    marlowe.doHaddock = meta.enableHaddock;
    marlowe.flags.defer-plugin-errors = meta.enableHaddock;

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

    # Note: The following two statements say that these tests should
    # _only_ run on linux. In actual fact we just don't want them
    # running on the 'mac-mini' instances, because these tests time out
    # there. In an ideal world this would be reflected here more
    # accurately.
    # TODO: Resolve this situation in a better way.
    marlowe.components.tests.marlowe-test-long-running = {
      platforms = lib.platforms.linux;
    };

    marlowe.ghcOptions = [ "-Werror" ];
    marlowe-actus.ghcOptions = [ "-Werror" ];
    marlowe-chain-sync.ghcOptions = [ "-Werror" ];
    marlowe-cli.ghcOptions = [ "-Werror" ];
    marlowe-contracts.ghcOptions = [ "-Werror" ];
    marlowe-integration.ghcOptions = [ "-Werror" ];
    marlowe-integration-tests.ghcOptions = [ "-Werror" ];
    marlowe-protocols.ghcOptions = [ "-Werror" ];
    marlowe-runtime.ghcOptions = [ "-Werror" ];
    marlowe-runtime-cli.ghcOptions = [ "-Werror" ];
    marlowe-runtime-web.ghcOptions = [ "-Werror" ];
    marlowe-test.ghcOptions = [ "-Werror" ];
  };

  modules = [{ inherit packages; }];
in

{
  inherit modules;
}

