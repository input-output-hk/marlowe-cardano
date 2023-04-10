{ system
, packages
}:
let
  inherit (packages) pkgs marlowe docs dev-scripts network;
  inherit (dev-scripts) nix-flakes-alias start-cardano-node mkCabalExeScript;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt writeShellScriptBin networks;
  inherit (marlowe) haskell cabal-install stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cardano-address;
  inherit (marlowe) writeShellScriptBinInRepoRoot;

  scriv = import ./nix/pkgs/scriv.nix { inherit pkgs; };

  set-xdg = ''
    export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
    mkdir -p "''${XDG_DATA_HOME}"
    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-''${HOME}/.local/run}"
    mkdir -p "''${XDG_RUNTIME_DIR}"
  '';

  marlowe-runtime-cli = mkCabalExeScript "marlowe-runtime-cli" "marlowe-runtime-cli";
  marlowe-cli = mkCabalExeScript "marlowe-cli" "marlowe-cli";

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
        excludes = [ ".*/spago-packages.nix$" ".*/packages.nix$" ];
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
  #
  # IMPORTANT: Do not add git to the dev shell, It breaks Lorri. See https://github.com/input-output-hk/plutus/pull/2422
  devToolsInputs = (with marlowe; [
    cabal-install
    pkgs.curl
    docs.build-and-serve-docs
    pkgs.editorconfig-core-c
    fix-prettier
    fixStylishHaskell
    pkgs.ghcid
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    pkgs.jq
    nix-flakes-alias
    nixpkgs-fmt
    pkgs.openssl
    pkgs.pkg-config
    pkgs.pre-commit
    pkgs.shellcheck
    pkgs.sqlite-interactive
    pkgs.haskellPackages.hspec-golden
    stylish-haskell
    marlowe-runtime-cli
    pkgs.yq
    pkgs.zlib
    pkgs.z3

    pkgs.docker-compose
    pkgs.sqitchPg
    pkgs.postgresql
    pkgs.json2yaml
    pkgs.yaml2json
    scriv
  ]);

  defaultShellHook = ''
    ${pre-commit-check.shellHook}

    # The green prompt familiar to those used to nix-shell
    export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] "
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$

    # Set up the compose file.
    # Can't run on darwin without Linux builds set up or if it's in hydra...
    nix run .#refresh-compose
  '';
  cardano-cli = pkgs.cardano.packages.cardano-cli;
  cardano-node = pkgs.cardano.packages.cardano-node;

  defaultShell = haskell.project.shellFor {
    buildInputs = devToolsInputs ++ (with marlowe; [
      cabal-install
      cardano-address
      cardano-cli
      cardano-node
      marlowe-cli
      start-cardano-node
      sphinxTools
      pkgs.docker-compose
      pkgs.postgresql
      pkgs.sqitchPg
      # FIXME: I'm not sure why I'm not able to grap rPackages here
    ]); # ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));
    # We don't currently use this, and it's a pain to materialize, and otherwise
    # costs a fair bit of eval time.
    withHoogle = false;
    shellHook = ''
      export PGUSER=postgres
      ${defaultShellHook}
    '';
  };

  develShells =
    let
      marloweCoreBuildInputs = devToolsInputs;
      marloweCliBuildInputs = devToolsInputs ++ [
        cardano-address
        cardano-node
        cardano-cli
        start-cardano-node
      ];
      libs = [
        pkgs.glibcLocales
        pkgs.libsodium-vrf
        pkgs.lzma
        pkgs.openssl_3_0.dev
        pkgs.secp256k1
        pkgs.zlib
      ] ++ pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

      develShell = { packages, buildInputs, name, shellHook ? "" }:
        haskell.project.shellFor {
          name = "marlowe-core-shell";
          packages = packages;
          buildInputs = libs ++ buildInputs;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          shellHook = ''
            export MARLOWE_NIX_SHELL="${name}"
            ${defaultShellHook}
            ${shellHook}
          '';
          withHoogle = true;

        };
    in
    {
      marloweCli = develShell { buildInputs = marloweCliBuildInputs; name = "cli"; packages = cmps: [ cmps.marlowe-cli ]; };
      marloweCore = develShell { buildInputs = marloweCoreBuildInputs; name = "core"; packages = cmps: [ cmps.marlowe ]; };
    };
in
defaultShell // {
  marlowe-cli = develShells.marloweCli;
  marlowe-core = develShells.marloweCore;
}
