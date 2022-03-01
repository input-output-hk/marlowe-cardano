{ pkgs
, gitignore-nix
, haskell
, webCommon
, webCommonMarlowe
, buildPursPackage
, buildNodeModules
, filterNpm
, purs-tidy
, prettier
, writeShellScriptBinInRepoRoot
}:
let
  playground-exe = haskell.packages.marlowe-playground-server.components.exes.marlowe-playground-server;

  build-playground-exe = "$(nix-build ../default.nix -A marlowe.haskell.packages.marlowe-playground-server.components.exes.marlowe-playground-server \"$@\")";

  build-ghc-with-marlowe = "$(nix-build --quiet --no-build-output -E '(import ./.. {}).marlowe.haskell.project.ghcWithPackages(ps: [ ps.marlowe ])' \"$@\")";

  # Output containing the purescript bridge code
  generated-purescript = pkgs.runCommand "marlowe-playground-purescript" { } ''
    mkdir $out
    ${playground-exe}/bin/marlowe-playground-server psgenerator $out
    cp ${builtins.path { name = "tidyrc.json"; path = ../.tidyrc.json; } } $out/.tidyrc.json
    cp ${builtins.path { name = "tidyoperators"; path = ../.tidyoperators; } } $out/.tidyoperators
    cd $out
    ${purs-tidy}/bin/purs-tidy format-in-place $out
    ${prettier}/bin/prettier -w $out
    rm $out/.tidyrc.json
    rm $out/.tidyoperators
  '';

  generate-purescript = writeShellScriptBinInRepoRoot "marlowe-playground-generate-purs" ''
    generated=./marlowe-playground-client/generated
    rm -rf $generated
    cp -a $(nix-build -A marlowe-playground.generated-purescript --no-out-link "$@") $generated
    chmod -R +w $generated
  '';

  # start-backend: script to start the plutus-playground-server
  #
  # Note-1: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  # Note-2: We want to avoid to pull the huge closure in so we use $(nix-build) instead
  start-backend = pkgs.writeShellScriptBin "marlowe-playground-server" ''
    echo "marlowe-playground-server: for development use only"
    GHC_WITH_PKGS=${build-ghc-with-marlowe}
    export PATH=$GHC_WITH_PKGS/bin:$PATH
    export FRONTEND_URL=https://localhost:8009

    ${build-playground-exe}/bin/marlowe-playground-server webserver
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = {
      shmish111.nearley-webpack-loader."939360f9d1bafa9019b6ff8739495c6c9101c4a1" = "1brx669dgsryakf7my00m25xdv7a02snbwzhzgc9ylmys4p8c10x";
    };
  };

  build-client = writeShellScriptBinInRepoRoot "marlowe-play-spago" ''
    cd marlowe-playground-client
    spago build --purs-args "--strict --stash --censor-lib --stash --is-lib=generated --is-lib=.spago"
  '';

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      checkPhase = ''
        ${pkgs.nodejs}/bin/npm run test
      '';
      name = "marlowe-playground-client";
      extraSrcs = {
        web-common-marlowe = webCommonMarlowe;
      };
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
      WEB_COMMON_MARLOWE_SRC = webCommonMarlowe;
    });
in
{
  inherit client generated-purescript generate-purescript start-backend build-client;
  server = playground-exe;
}
