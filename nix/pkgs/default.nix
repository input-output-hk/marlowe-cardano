# This is meant to be called from:
#  * nix/default.nix
{ pkgs
, system
, evalSystem
, config ? { allowUnfreePredicate = (import ../lib/unfree.nix).unfreePredicate; }
, inputs
, enableHaskellProfiling
, source-repo-override
}:
let
  inherit (pkgs) stdenv;

  # { index-state, compiler-nix-name, project, projectPackages, packages, extraPackages }
  haskell = pkgs.callPackage ./haskell {
    inherit inputs evalSystem;
    inherit enableHaskellProfiling;
    inherit source-repo-override;

    # This ensures that the utility scripts produced in here will run on the current system, not
    # the build system, so we can run e.g. the darwin ones on linux
    inherit (pkgs.evalPackages) writeShellScript;
  };

  #
  # additional haskell packages from ./nix/pkgs/haskell-extra
  #
  exeFromExtras = x: haskell.extraPackages."${x}".components.exes."${x}";
  cabal-install = haskell.extraPackages.cabal-install.components.exes.cabal;
  stylish-haskell = exeFromExtras "stylish-haskell";
  hlint = exeFromExtras "hlint";
  haskell-language-server = exeFromExtras "haskell-language-server";
  haskell-language-server-wrapper = pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''${haskell-language-server}/bin/haskell-language-server "$@"'';
  hie-bios = exeFromExtras "hie-bios";

  inherit (haskell.project.hsPkgs.cardano-addresses-cli.components.exes) cardano-address;

  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';

  fixStylishHaskell = pkgs.callPackage ./fix-stylish-haskell.nix { inherit stylish-haskell; };

  #
  # sphinx python packages
  #
  sphinx-markdown-tables = pkgs.python3Packages.callPackage ./sphinx-markdown-tables.nix { };
  sphinxemoji = pkgs.python3Packages.callPackage ./sphinxemoji.nix { };

  nix-pre-commit-hooks = inputs.pre-commit-hooks.lib.${system};

  # sphinx haddock support
  sphinxcontrib-haddock = pkgs.callPackage (inputs.sphinxcontrib-haddock) { pythonPackages = pkgs.python3Packages; };

  formatting = pkgs.callPackage ./formatting.nix {
    inherit writeShellScriptBinInRepoRoot;
  };


  # combined haddock documentation for all public plutus libraries
  plutus-haddock-combined =
    let
      haddock-combine = pkgs.callPackage (inputs.plutus-core + "/nix/lib/haddock-combine.nix") {
        ghc = haskell.projectAllHaddock.pkg-set.config.ghc.package;
        inherit (sphinxcontrib-haddock) sphinxcontrib-haddock;
      };
    in
    pkgs.callPackage (inputs.plutus-core + "/nix/pkgs/plutus-haddock-combined") {
      inherit haskell haddock-combine;
      inherit (pkgs) haskell-nix;
    };

  # Collect everything to be exported under `plutus.lib`: builders/functions/utils
  lib = rec {
    haddock-combine = pkgs.callPackage (inputs.plutus-core + "/nix/lib/haddock-combine.nix") { inherit sphinxcontrib-haddock; };
  };

in
{
  inherit sphinx-markdown-tables sphinxemoji sphinxcontrib-haddock;
  inherit nix-pre-commit-hooks;
  inherit haskell cabal-install stylish-haskell hlint haskell-language-server haskell-language-server-wrapper hie-bios cardano-address;
  inherit fixStylishHaskell writeShellScriptBinInRepoRoot;
  inherit plutus-haddock-combined;
  inherit lib;
  inherit (formatting) fix-prettier;
}
