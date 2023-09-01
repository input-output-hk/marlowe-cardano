# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ nix, inputs, inputs', pkgs, project, l, ... }:

let

  scripts = nix.marlowe-cardano.scripts;

  isLinux = pkgs.stdenv.hostPlatform.isLinux;

in

{
  name = "marlowe-cardano";


  packages = [
    inputs'.cardano-world.cardano.packages.cardano-address
    inputs'.cardano-world.cardano.packages.cardano-node
    inputs'.cardano-world.cardano.packages.cardano-cli

    pkgs.z3
    pkgs.sqitchPg
    pkgs.postgresql
    pkgs.haskellPackages.hspec-golden
    pkgs.jq
    pkgs.docker-compose
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
  };


  enterShell = l.optionalString isLinux "refresh-compose";
}
