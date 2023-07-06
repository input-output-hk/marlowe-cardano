# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ inputs, inputs', pkgs, projects }:

let

  system = pkgs.stdenv.system;

in

{
  operables = import ./marlowe-cardano/deploy/operables.nix
    { inherit inputs pkgs; };


  oci-images = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (
    import ./marlowe-cardano/deploy/oci-images.nix { inherit inputs pkgs; }
  );


  nomadTasks = import ./marlowe-cardano/deploy/nomadTasks.nix
    { inherit inputs; };


  networks = import ./marlowe-cardano/networks.nix
    { inherit inputs pkgs; };


  packages.marlowe-integration-tests = import ./marlowe-cardano/integration-tests.nix
    { inherit inputs pkgs projects; };


  packages.marlowe-chain-indexer = inputs.self.packages.marlowe-chain-sync-exe-marlowe-chain-indexer-ghc8107;
  packages.marlowe-chain-sync = inputs.self.packages.marlowe-chain-sync-exe-marlowe-chain-sync-ghc8107;
  packages.marlowe = inputs.self.packages.marlowe-runtime-exe-marlowe-runtime-ghc8107;
  packages.marlowe-tx = inputs.self.packages.marlowe-runtime-exe-marlowe-tx-ghc8107;
  packages.marlowe-indexer = inputs.self.packages.marlowe-runtime-exe-marlowe-indexer-ghc8107;
  packages.marlowe-sync = inputs.self.packages.marlowe-runtime-exe-marlowe-sync-ghc8107;
  packages.marlowe-cli = inputs.self.packages.marlowe-cli-exe-marlowe-cli-ghc8107;
}
