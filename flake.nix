{
  description = "Marlowe Cardano implementation";

  inputs = {
    cardano-world.url = "github:input-output-hk/cardano-world/d22f50fc77d23e2612ca2b313a098dd0b48834d4";
    std.url = "github:divnix/std";
    iogx.url = "github:input-output-hk/iogx";
    n2c.url = "github:nlewo/nix2container";
    bitte-cells.url = "github:input-output-hk/bitte-cells";
  };

  outputs = inputs:
    inputs.iogx.mkFlake {
      inherit inputs;
      debug = true;
      repoRoot = ./.;
      flakeOutputsPrefix = "";
      systems = [ "x86_64-darwin" "x86_64-linux" ];
      haskellCompilers = [ "ghc8107" ];
      defaultHaskellCompiler = "ghc8107";
      haskellCrossSystem = null;
      haskellProjectFile = ./nix/haskell-project.nix;
      perSystemOutputsFile = ./nix/per-system-outputs.nix;
      shellPrompt = "\n\\[\\033[1;32m\\][marlowe-cardano:\\w]\\$\\[\\033[0m\\] ";
      shellWelcomeMessage = "🤟 \\033[1;31mWelcome to marlowe-cardano\\033[0m 🤟";
      shellModuleFile = ./nix/shell-module.nix;
      includeHydraJobs = true;
      excludeProfiledHaskellFromHydraJobs = true;
      blacklistedHydraJobs = [ ];
      enableHydraPreCommitCheck = false;
      readTheDocsSiteDir = null;
      readTheDocsHaddockPrologue = "";
      readTheDocsExtraHaddockPackages = _: { };
      preCommitCheckHooks = { editorconfig-checker.enable = false; };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}
