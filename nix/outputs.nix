{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;


  staticPkgs = project.cabalProject.projectCross.musl64.hsPkgs;


  static =
    staticPkgs.marlowe-apps.components.exes //
    staticPkgs.marlowe-cli.components.exes //
    staticPkgs.marlowe-runtime-cli.components.exes;


  allStatic = pkgs.runCommand "all-statics" { } ''
    mkdir -p $out
    ${lib.concatMapStringsSep "\n" (drv: "cp ${drv}/bin/* $out") (lib.attrValues static)}
  '';

in

[
  # Default packages, apps, checks, devShells, hydraJobs for the Haskell project.
  (
    project.flake
  )

  # Extra flake outputs
  {
    inherit static allStatic;

    operables = repoRoot.nix.marlowe-cardano.deploy.operables;
    oci-images = repoRoot.nix.marlowe-cardano.deploy.oci-images;
    nomadTasks = repoRoot.nix.marlowe-cardano.deploy.nomadTasks;

    networks = repoRoot.nix.marlowe-cardano.networks;

    packages.integration-tests = repoRoot.nix.marlowe-cardano.integration-tests;

    checks.check-validators = repoRoot.nix.marlowe-cardano.check-validators;

    hydraJobs.operables = repoRoot.nix.marlowe-cardano.deploy.operables;
    hydraJobs.oci-images = repoRoot.nix.marlowe-cardano.deploy.oci-images;
  }

  # hydraJobs for linux only
  (lib.optionalAttrs pkgs.stdenv.isLinux {
    hydraJobs.static = static;
    hydraJobs.allStatic = allStatic;
  })
]
