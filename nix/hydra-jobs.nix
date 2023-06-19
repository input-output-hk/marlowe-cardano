{ inputs, inputs', pkgs, flake }:

{
  excludedPaths = [
    "packages.ghc8107-profiled"
    "packages.ghc8107-mingwW64-profiled"
    "devShells.ghc8107-profiled"
    "devShells.ghc8107-mingwW64-profiled"
    "checks.ghc8107-profiled"
    "checks.ghc8107-mingwW64-profiled"
    "networks"
    "nomadTasks"
    "operables"
  ];

  extraJobs = { };
}
