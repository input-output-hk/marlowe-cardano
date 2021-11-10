{ pkgs, gitignore-nix, haskell, webCommon, webCommonMarlowe, buildPursPackage, buildNodeModules, filterNpm }:
let
  marlowe-invoker = haskell.packages.marlowe.components.exes.marlowe-pab;

  pab-setup-invoker = haskell.packages.plutus-pab.components.exes.plutus-pab-setup;

  generated-purescript = pkgs.runCommand "marlowe-pab-purescript" { } ''
    mkdir $out
    ${pab-setup-invoker}/bin/plutus-pab-setup psgenerator $out
    ln -s ${./plutus-pab.yaml} plutus-pab.yaml
    ${marlowe-invoker}/bin/marlowe-pab --config plutus-pab.yaml psapigenerator $out
  '';

  generate-purescript = pkgs.writeShellScriptBin "marlowe-pab-generate-purs" ''
    generatedDir=./generated
    rm -rf $generatedDir
    $(nix-build ../default.nix -A marlowe-dashboard.pab-setup-invoker)/bin/plutus-pab-setup psgenerator $generatedDir
    $(nix-build ../default.nix -A marlowe-dashboard.marlowe-invoker)/bin/marlowe-pab --config plutus-pab.yaml psapigenerator $generatedDir
  '';

  start-backend = pkgs.writeShellScriptBin "marlowe-pab-server" ''
    echo "marlowe-pab-server: for development use only"
    $(nix-build ../default.nix --quiet --no-build-output -A marlowe-dashboard.marlowe-invoker)/bin/marlowe-pab --config plutus-pab.yaml all-servers
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = { };
  };

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      name = "marlowe-dashboard-client";
      extraSrcs = {
        web-common-marlowe = webCommonMarlowe;
        generated = generated-purescript;
      };
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
    });
in
{
  inherit client marlowe-invoker pab-setup-invoker generate-purescript generated-purescript start-backend;
}
