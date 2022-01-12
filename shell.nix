{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, packages ? import ./. { inherit system enableHaskellProfiling; }
}:
let
  inherit (packages) pkgs marlowe marlowe-playground marlowe-dashboard docs webCommon webCommonPlayground bitte-packages marlowe-cli;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt writeShellScriptBin;
  inherit (marlowe) haskell stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cardano-cli cardano-node;
  inherit (marlowe) purs-tidy-hook prettier-hook;

  set-xdg = ''
    export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
    mkdir -p "''${XDG_DATA_HOME}"
    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-''${HOME}/.local/run}"
    mkdir -p "''${XDG_RUNTIME_DIR}"
  '';

  launch-node = writeShellScriptBin "launch-node" ''
    set -eEuo pipefail

    ${set-xdg}

    export NODE_STATE_DIR="''${NODE_STATE_DIR:-''${XDG_DATA_HOME}/node}"
    mkdir -p "$NODE_STATE_DIR"

    export NOMAD_ALLOC_DIR="''${NOMAD_ALLOC_DIR:-''${XDG_RUNTIME_DIR}}"

    export NOMAD_PORT_node="''${NOMAD_PORT_node:-3001}"

    exec -a entrypoint ${bitte-packages.node}/bin/entrypoint
  '';

  launch-chain-index = writeShellScriptBin "launch-chain-index" ''
    set -eEuo pipefail

    ${set-xdg}

    export INDEX_STATE_DIR="''${INDEX_STATE_DIR:-''${XDG_DATA_HOME}/index}"
    mkdir -p "$INDEX_STATE_DIR"

    export NOMAD_ALLOC_DIR="''${NOMAD_ALLOC_DIR:-''${XDG_RUNTIME_DIR}}"

    export NOMAD_PORT_index="''${NOMAD_PORT_index:-9083}"

    exec -a entrypoint ${bitte-packages.chain-index}/bin/entrypoint
  '';

  launch-wbe = writeShellScriptBin "launch-wbe" ''
    set -eEuo pipefail

    ${set-xdg}

    export NOMAD_ALLOC_DIR="''${NOMAD_ALLOC_DIR:-''${XDG_RUNTIME_DIR}}"

    export NOMAD_PORT_wbe="''${NOMAD_PORT_wbe:-8090}"

    exec -a entrypoint ${bitte-packages.wbe}/bin/entrypoint
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
      inherit purs-tidy-hook;
      prettier = prettier-hook;
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

  nixFlakesAlias = pkgs.runCommand "nix-flakes-alias" { } ''
    mkdir -p $out/bin
    ln -sv ${pkgs.nixFlakes}/bin/nix $out/bin/nix-flakes
  '';

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = (with pkgs; [
    cacert
    editorconfig-core-c
    ghcid
    jq
    nixFlakesAlias
    nixpkgs-fmt
    nodejs
    shellcheck
    sqlite-interactive
    stack
    yq
    z3
    zlib
    nodePackages.prettier
  ] ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with marlowe; [
    cabal-install
    cardano-repo-tool
    fixPngOptimization
    fix-prettier
    fix-purs-tidy
    fixStylishHaskell
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    marlowe-dashboard.generate-purescript
    marlowe-dashboard.start-backend
    marlowe-playground.generate-purescript
    marlowe-playground.start-backend
    purs
    purs-tidy
    spago
    psa
    purescript-language-server
    spago2nix
    stylish-haskell
    updateMaterialized
    updateClientDeps
    docs.build-and-serve-docs

    launch-node
    launch-chain-index
    launch-wbe
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs ++ [ sphinxTools ];
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  ''
  # Point to some source dependencies
  + ''
    export ACTUS_TEST_DATA_DIR=${packages.actus-tests}/tests/
    export WEB_COMMON_SRC="${webCommon.cleanSrc}"
    export WEB_COMMON_PLAYGROUND_SRC="${webCommonPlayground}"
  '';
}
