# The actual flake outputs, per system.
# 
# This is where you define extra `packages`, `checks`, `apps`, etc..., or any 
# non-standard flake output like `nomadTasks` or `operables`.
#
# Remember that you can access these using `self` from `inputs` or 
# `systemized-inputs`, for example:
#   `inputs.self.nomadTasks`
#   `systemized-inputs.self.nomadTasks.x86_64-linux`
#
# iogx will union its outputs with yours, and yours will take precedence.
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
{
  operables = import ./marlowe-cardano/deploy/operables.nix
    { inherit inputs pkgs; };

  oci-images = import ./marlowe-cardano/deploy/oci-images.nix
    { inherit inputs pkgs; };

  nomadTasks = import ./marlowe-cardano/deploy/nomadTasks.nix
    { inherit inputs; };

  networks = import ./marlowe-cardano/networks.nix
    { inherit inputs pkgs; };

  packages.entrypoints = import ./marlowe-cardano/bitte
    { inherit inputs pkgs; };
}
