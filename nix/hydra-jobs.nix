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
    # FIXME unsuppress pre-commit-check once fourmolu and editorconfig
    # have been fully integrated
    "packages.ghc8107.pre-commit-check"
    "packages.ghc8107-mingwW64.pre-commit-check"

    # FIXME remove once they are fixed
    "packages.entrypoints.testnet-pioneers.node"
    "packages.entrypoints.testnet-dev.node"

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
