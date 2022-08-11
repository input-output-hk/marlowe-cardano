{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:
let
  inherit (packages) pkgs marlowe docs marlowe-cli dev-scripts network;
  inherit (dev-scripts) nix-flakes-alias start-cardano-node run-chainseekd;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt writeShellScriptBin networks;
  inherit (marlowe) haskell stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cardano-cli cardano-node;
  inherit (marlowe) writeShellScriptBinInRepoRoot;

  set-xdg = ''
    export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
    mkdir -p "''${XDG_DATA_HOME}"
    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-''${HOME}/.local/run}"
    mkdir -p "''${XDG_RUNTIME_DIR}"
  '';

  # For Sphinx, and ad-hoc usage
  sphinxTools = python3.withPackages (ps: [
    sphinxcontrib-haddock.sphinxcontrib-domaintools
    sphinx-markdown-tables
    sphinxemoji
    ps.sphinxcontrib_plantuml
    ps.sphinxcontrib-bibtex
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.recommonmark
  ]);

  # Configure project pre-commit hooks
  pre-commit-check = nix-pre-commit-hooks.run {
    src = (lib.cleanSource ./.);
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
    };
    hooks = {
      prettier = {
        enable = true;
        types_or = [ "javascript" "css" "html" ];
      };
      stylish-haskell.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/pkgs/haskell/materialized.*/.*" ".*/spago-packages.nix$" ".*/packages.nix$" ];
      };
      shellcheck.enable = true;
      png-optimization = {
        enable = true;
        name = "png-optimization";
        description = "Ensure that PNG files are optimized";
        entry = "${pkgs.optipng}/bin/optipng";
        files = "\\.png$";
      };
    };
  };

  # marlowe and subproject independent dev tools
  # ---------------------------------------------------------------------------
  # IMPORTANT: Do not add git to the dev shell, It breaks Lorri!
  # ---------------------------------------------------------------------------
  devToolsInputs = (with pkgs; with marlowe; [
    coreutils
    cabal-install
    curl
    docs.build-and-serve-docs
    editorconfig-core-c
    fix-prettier
    fixStylishHaskell
    ghc
    ghcid
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    jq
    nix-flakes-alias
    nixpkgs-fmt
    openssl
    pkg-config
    pre-commit
    shellcheck
    sqlite-interactive
    stylish-haskell
    tmux
    updateMaterialized
    yq
    zlib
    z3
  ]);

  defaultShellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  '';

  defaultShell = haskell.project.shellFor {
    buildInputs = devToolsInputs ++ (with marlowe; [
      cabal-install
      cardano-cli
      cardano-node
      cardano-repo-tool
      marlowe-cli
      run-chainseekd
      start-cardano-node
      sphinxTools
      # FIXME: I'm not sure why I'm not able to grap rPackages here
    ]); # ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

    # We don't currently use this, and it's a pain to materialize, and otherwise
    # costs a fair bit of eval time.
    withHoogle = false;
    shellHook = ''
      export ACTUS_TEST_DATA_DIR=${packages.actus-tests}/tests/
      export PGUSER=postgres
      ${defaultShellHook}
    '';
  };

  develShells =
    let

      marloweCoreBuildInputs = devToolsInputs;
      marloweCliBuildInputs = devToolsInputs ++ [
        cardano-node
        cardano-cli
        start-cardano-node
      ];
      develShell = { buildInputs, name, shellHook ? "" }:
        let
          libs = [
            pkgs.glibcLocales
            pkgs.libsodium-vrf
            pkgs.lzma
            pkgs.openssl_3_0.dev
            pkgs.secp256k1
            pkgs.zlib
          ] ++ pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];
        in
        pkgs.mkShell {
          name = "marlowe-core-shell";
          buildInputs = libs ++ buildInputs;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          shellHook = ''
            export MARLOWE_NIX_SHELL="${name}"
            ${defaultShellHook}
            ${shellHook}
          '';
        };
    in
    {
      marloweActus = develShell {
        buildInputs = marloweCoreBuildInputs;
        name = "actus";
        shellHook = ''
          export ACTUS_TEST_DATA_DIR=${packages.actus-tests}/tests/
        '';
      };
      marloweCli = develShell { buildInputs = marloweCliBuildInputs; name = "cli"; };
      marloweCore = develShell { buildInputs = marloweCoreBuildInputs; name = "core"; };
    };
in
defaultShell // {
  marlowe-actus = develShells.marloweActus;
  marlowe-cli = develShells.marloweCli;
  marlowe-core = develShells.marloweCore;
}
