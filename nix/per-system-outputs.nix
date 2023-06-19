{ inputs, inputs', pkgs, projects }:

let

  system = pkgs.stdenv.system;

in

{
  operables = import ./marlowe-cardano/deploy/operables.nix
    { inherit inputs pkgs; };


  oci-images = import ./marlowe-cardano/deploy/oci-images.nix
    { inherit inputs pkgs; };


  nomadTasks = import ./marlowe-cardano/deploy/nomadTasks.nix
    { inherit inputs; };


  networks = import ./marlowe-cardano/networks.nix
    { inherit inputs pkgs; };


  packages.entrypoints = import ./marlowe-cardano/bitte/default.nix
    { inherit inputs pkgs; };


  packages.marlowe-integration-tests = import ./marlowe-cardano/integration-tests.nix
    { inherit inputs pkgs projects; };
}
