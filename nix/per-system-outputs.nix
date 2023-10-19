# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ nix, inputs, inputs', pkgs, projects, l, system, ... }:

let

  staticPkgs = inputs'.self.packages.marlowe-apps-exe-marlowe-finder.project.projectCross.musl64.hsPkgs;

in
rec
{
  docker-tools = nix.docker.default;

  oci-images =
    l.optionalAttrs pkgs.stdenv.hostPlatform.isLinux
      nix.marlowe-cardano.deploy.oci-images;

  static = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux
    (
      staticPkgs.marlowe-apps.components.exes //
      staticPkgs.marlowe-cli.components.exes //
      staticPkgs.marlowe-runtime-cli.components.exes
    );

  allStatic = pkgs.runCommand "all-statics" { } ''
    mkdir -p $out
    ${l.concatMapStringsSep "\n" (drv: "cp ${drv}/bin/* $out") (l.attrValues static)}
  '';

  nomadTasks = nix.marlowe-cardano.deploy.nomadTasks;


  networks = nix.marlowe-cardano.networks;


  packages.integration-tests = nix.marlowe-cardano.integration-tests { inherit projects; };

  checks = {
    check-validators = nix.marlowe-cardano.check-validators;
  };
}
