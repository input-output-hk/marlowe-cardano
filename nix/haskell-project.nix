# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixhaskell-projectnix

{ inputs, inputs', meta, config, pkgs, lib }:

let

  lib = pkgs.lib;

  mkIfDarwin = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin;

  packages = {

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

  modules = [{ inherit packages; }];

in

{
  inherit modules;
}

