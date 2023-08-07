# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ inputs, inputs', pkgs, projects, ... }:

let

  staticPkgs = inputs.self.packages.marlowe-apps-exe-marlowe-finder-ghc8107.project.projectCross.musl64.hsPkgs;

in
{
  operables = import ./marlowe-cardano/deploy/operables.nix
    { inherit inputs pkgs; };

  oci-images = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (
    import ./marlowe-cardano/deploy/oci-images.nix { inherit inputs pkgs; }
  );

  static = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux
    (
      staticPkgs.marlowe-apps.components.exes //
      staticPkgs.marlowe-cli.components.exes //
      staticPkgs.marlowe-runtime-cli.components.exes
    );

  nomadTasks = import ./marlowe-cardano/deploy/nomadTasks.nix
    { inherit inputs; };


  networks = import ./marlowe-cardano/networks.nix
    { inherit inputs pkgs; };


  packages.marlowe-integration-tests = import ./marlowe-cardano/integration-tests.nix
    { inherit inputs' pkgs projects; };
}
