# Your development shell is defined here.
# You can add packages, custom scripts and a shell hook.
{
  # Desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the 
  # inputs defined in your flake. You will also find the `self` attribute here.
  # These inputs have been desystemized against the current system.
  inputs

  # Non-desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the 
  # inputs defined in your flake. You will also find the `self` argument here. 
  # These inputs have not been desystemized, they are the original `inputs` from
  # iogx and your `flake.nix`.
, systemized-inputs

  # The very config passed as second argument to `inputs.iogx.mkFlake` in your 
  # `flake.nix`.
, flakeopts

  # Desystemized legacy nix packages configured against `haskell.nix`.
  # NEVER use the `nixpkgs` coming from `inputs` or `systemized-inputs`!
, pkgs
}:

let
  scripts = import ./marlowe-cardano/scripts.nix { inherit inputs pkgs; };
in
{
  packages = [
    inputs.cardano-world.cardano.packages.cardano-address
    inputs.cardano-world.cardano.packages.cardano-node
    inputs.cardano-world.cardano.packages.cardano-cli
  ];

  enterShell = ''
    export PGUSER=postgres
  '';

  scripts = {
    re-up = {
      description = "re-up";
      exec = scripts.re-up;
      enabled = pkgs.stdenv.system == "x86_64-linux";
    };
    start-cardano-node.exec = scripts.start-cardano-node;
  };
}
