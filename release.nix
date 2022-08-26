# Temporary until we can migrate the IO hydra to treat this as a flake repo
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, rootsOnly ? false
  # We explicitly pass true here in the GitHub action but don't want to slow down hydra
, checkMaterialization ? false
, plutus-apps ? null
, marlowe-cardano ? null
, evalSystem ? builtins.currentSystem
}@args:
let
  flake = builtins.getFlake "path:${toString ./.}";
in
flake.hydraJobs ({
  # in { foo ? bar }@baz, baz will not have foo if foo was defaulted. We want to override hydra-jobs.nix's default evalSystem.
  inherit evalSystem;
} // args)
