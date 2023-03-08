# Temporary until we can migrate the github actions to use the flake interface to jobs
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, rootsOnly ? false
, plutus-apps ? null
, evalSystem ? builtins.currentSystem
}@args:
let
  flake = builtins.getFlake "path:${toString ./.}";
in
flake.hydraJobs ({
  # in { foo ? bar }@baz, baz will not have foo if foo was defaulted. We want to override hydra-jobs.nix's default evalSystem.
  inherit evalSystem;
} // args)
