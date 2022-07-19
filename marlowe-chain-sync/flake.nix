{
  description = "Marlowe Chain Sync";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hls-source = {
      # Pinned to a release
      url = "github:haskell/haskell-language-server?ref=1.3.0";
      flake = false;
    };
  };

  outputs = { self, haskellNix, nixpkgs, utils, gitignore, hls-source, pre-commit-hooks, iohkNix, ... }:
    let
      inherit (gitignore.lib) gitignoreSource;
      inherit (utils.lib) eachSystem defaultSystems;

      src = gitignoreSource ./.;

      mkFlake = system:
        let
          projectOverlay = self: super:
            let
              haskell = import ./nix/haskell.nix {
                inherit src pkgs hls-source;
              };
            in
            {
              inherit (haskell) project haskell-language-server hie-bios implicit-hie stylish-haskell hlint cabal-install;
            };
          overlays = [
            haskellNix.overlay
            iohkNix.overlays.cardano-lib
            projectOverlay
          ];
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit (pkgs) mkShell;
          flake = pkgs.project.flake { };

          # Configure project pre-commit hooks
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            inherit src;
            tools = {
              stylish-haskell = pkgs.stylish-haskell.components.exes.stylish-haskell;
              nixpkgs-fmt = pkgs.nixpkgs-fmt;
            };
            hooks = {
              stylish-haskell.enable = true;
              nixpkgs-fmt = {
                enable = true;
                # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
                # does not appear to work inside the hook. For now we have to thus
                # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
                excludes = [ ".*nix/pkgs/haskell/materialized.*/.*" ".*/spago-packages.nix$" ".*/packages.nix$" ];
              };
            };
          };
          run-node = import ./nix/cardano-node.nix {
            inherit pkgs;
            port = 3001;
            network = with pkgs.cardanoLib; with environments; testnet // {
              topology = mkEdgeTopology {
                edgeNodes = [ testnet.relaysNew ];
                edgePort = testnet.edgePort;
                valency = 1;
              };
            };
          };

        in
        flake // {
          devShell = pkgs.project.shellFor {
            withHoogle = false;
            buildInputs = with pkgs; [
              nixpkgs-fmt
              run-node
              docker-compose
              postgresql
              sqitchPg
              hie-bios.components.exes.hie-bios
              haskell-language-server.components.exes.haskell-language-server
              hlint.components.exes.hlint
              stylish-haskell.components.exes.stylish-haskell
              cabal-install.components.exes.cabal
            ];
            shellHook = pre-commit-check.shellHook
            + ''
              export PGUSER=postgres
              export PGPASSWORD=9kZ@o7j4OkMC
            '';
          };
        };
    in
    eachSystem defaultSystems mkFlake;
}
