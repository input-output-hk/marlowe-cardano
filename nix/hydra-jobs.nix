{ inputs, inputs', pkgs }:

{
  includedPaths = [
    "packages"
    "devShells"
    "checks"

    "oci-images"
    "operables"
  ];

  excludedPaths = [
    "packages.ghc8107-mingwW64"
    "packages.ghc8107-profiled"
    "packages.ghc8107-mingwW64-profiled"

    "devShells.ghc8107-mingwW64"
    "devShells.ghc8107-profiled"
    "devShells.ghc8107-mingwW64-profiled"

    "checks.ghc8107-mingwW64"
    "checks.ghc8107-profiled"
    "checks.ghc8107-mingwW64-profiled"
  ];
}
