{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let

  scripts = repoRoot.nix.marlowe-cardano.scripts;

  isLinux = pkgs.stdenv.hostPlatform.isLinux;

in

{
  name = "marlowe-cardano";


  packages = [
    inputs.cardano-world.cardano.packages.cardano-address
    inputs.cardano-node.packages.cardano-node
    inputs.cardano-node.packages.cardano-cli

    inputs.marlowe-plutus.packages.marlowe-minting-validator

    pkgs.z3
    pkgs.sqitchPg
    pkgs.postgresql

    pkgs.jq
    pkgs.docker-compose

    cabalProject.hsPkgs.hspec-golden.components.exes.hgold
  ];


  env.PGUSER = "postgres";


  scripts = {

    re-up = {
      description = "Builds compose.nix, (re)creates and (re)starts the dev docker containers for Runtime.";
      exec = scripts.re-up;
      enable = isLinux;
      group = "marlowe";
    };

    refresh-compose = {
      description = "Genereate compose.yaml in the repository root";
      exec = scripts.refresh-compose;
      enable = isLinux;
      group = "marlowe";
    };

    marlowe-runtime-cli = {
      exec = scripts.marlowe-runtime-cli;
      description = "Marlowe Runtime CLI";
      group = "marlowe";
    };

    marlowe-cli = {
      exec = scripts.marlowe-cli;
      description = "Marlowe CLI";
      group = "marlowe";
    };

    refresh-validators = {
      exec = scripts.refresh-validators;
      description = "Pull the latest validators from the marlowe-plutus flake input.";
      group = "marlowe";
    };
  };


  shellHook = lib.optionalString isLinux "refresh-compose";


  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    fourmolu.enable = true;
    hlint.enable = true;
  };
}
