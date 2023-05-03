# Your development shell is defined here.
# You can add packages, scripts, envvars, and a shell hook.

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

  # The very attrset passed to `inputs.iogx.mkFlake` in your `flake.nix`.
, flakeopts

  # Desystemized legacy nix packages configured against `haskell.nix`.
  # NEVER use the `nixpkgs` coming from `inputs` or `systemized-inputs`!
, pkgs

  # A reference to the `haskell.nix` project on top of which this shell will be 
  # built. This can be used for example to bring some haskell executables into 
  # the shell:
  # packages = [
  #   haskell-nix-project.hsPkgs.cardano-cli.components.exes.cardano-cli
  #   haskell-nix-project.hsPkgs.cardano-node.components.exes.cardano-node
  # ];
  # Be careful not to refence the project's own haskell packages.
, haskell-nix-project
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

  env.PGUSER = "postgres";

  scripts = {
    re-up = {
      description = "re-up";
      exec = scripts.re-up;
      enabled = pkgs.stdenv.system == "x86_64-linux";
    };
    start-cardano-node.exec = scripts.start-cardano-node;
  };
}
