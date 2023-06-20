{ inputs, inputs', pkgs }:

{
  excludedPaths = [
    # FIXME unsuppress pre-commit-check once fourmolu and editorconfig
    # have been fully integrated
    "packages.ghc8107.pre-commit-check"
    "packages.ghc8107-mingwW64.pre-commit-check"

    "packages.ghc8107-profiled"
    "packages.ghc8107-mingwW64-profiled"

    "devShells.ghc8107-profiled"
    "devShells.ghc8107-mingwW64-profiled"

    "checks.ghc8107-profiled"
    "checks.ghc8107-mingwW64-profiled"
  ];
}
