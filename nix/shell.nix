# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

{ inputs, inputs', pkgs, project }:

let

  scripts = import ./marlowe-cardano/scripts.nix { inherit inputs pkgs project; };

  isLinux = pkgs.stdenv.hostPlatform.isLinux;

in

{
  name = "marlowe-cardano";


  packages = [
    inputs.cardano-world.cardano.packages.cardano-address
    inputs.cardano-world.cardano.packages.cardano-node
    inputs.cardano-world.cardano.packages.cardano-cli

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


  enterShell = pkgs.lib.optionalString isLinux "refresh-compose";
}
