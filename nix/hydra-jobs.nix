{ inputs, inputs', pkgs, flake }:

{
  packages = {
    inherit (flake.packages) ghc8107 entrypoints;
  };


  inherit (flake) oci-images networks;


  devShells = {
    inherit (flake.devShells) ghc8107;
  };


  checks = {
    inherit (flake.checks) ghc8107;
  };
}
