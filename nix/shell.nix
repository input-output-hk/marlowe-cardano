{ inputs, inputs', pkgs, project }:

let

  scripts = import ./marlowe-cardano/scripts.nix { inherit inputs pkgs project; };

  isLinux = pkgs.stdenv.hostPlatform.isLinux;

in

{
  name = "marlowe-cardano";


  prompt = "\n\\[\\033[1;32m\\][marlowe-cardano:\\w]\\$\\[\\033[0m\\] ";


  welcomeMessage = "🤟 \\033[1;31mWelcome to marlowe-cardano\\033[0m 🤟";


  packages = [
    inputs.cardano-world.cardano.packages.cardano-address
    inputs.cardano-world.cardano.packages.cardano-node
    inputs.cardano-world.cardano.packages.cardano-cli

    pkgs.sqitchPg
    pkgs.postgresql
  ];


  env.PGUSER = "postgres";


  scripts = {
    re-up = {
      description = "Builds compose.nix, (re)creates and (re)starts the dev docker containers for Runtime.";
      exec = scripts.re-up;
      enabled = isLinux;
      group = "marlowe";
    };

    refresh-compose = {
      description = "Genereate compose.yaml in the repository root";
      exec = scripts.refresh-compose;
      enabled = isLinux;
      group = "marlowe";
    };

    start-cardano-node = {
      exec = scripts.start-cardano-node;
      description = "Start cardano-node";
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
