{
  debug = true;
  repoRoot = ../.;
  systems = [ "x86_64-darwin" "x86_64-linux" ];
  haskellCompilers = [ "ghc8107" ];
  defaultHaskellCompiler = "ghc8107";
  shouldCrossCompile = true;
  haskellProjectFile = ./haskell-project.nix;
  perSystemOutputsFile = ./per-system-outputs.nix;
  topLevelOutputsFile = null;
  shellFile = ./shell.nix;
  hydraJobsFile = ./hydra-jobs.nix;
  readTheDocsFile = null;
  preCommitCheckFile = ./pre-commit-check.nix;
}
