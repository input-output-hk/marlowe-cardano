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
  marlowe-invoker = haskell.packages.marlowe.components.exes.marlowe-pab;

  marlowe-run-backend-invoker = haskell.packages.marlowe-dashboard-server.components.exes.marlowe-dashboard-server;
  psgenerator = haskell.packages.marlowe-dashboard-server.components.exes.psgenerator;

  generated-purescript = pkgs.runCommand "marlowe-pab-purescript" { } ''
    mkdir $out
    ${psgenerator}/bin/psgenerator $out
    cp ${builtins.path { name = "tidyrc.json"; path = ../.tidyrc.json; } } $out/.tidyrc.json
    cp ${builtins.path { name = "tidyoperators"; path = ../.tidyoperators; } } $out/.tidyoperators
    cd $out
    ${purs-tidy}/bin/purs-tidy format-in-place $out
    ${prettier}/bin/prettier -w $out
    rm $out/.tidyrc.json
    rm $out/.tidyoperators
  '';

  generate-purescript = writeShellScriptBinInRepoRoot "marlowe-run-generate-purs" ''
    generated=./marlowe-dashboard-client/generated
    rm -rf $generated
    cp -a $(nix build .#marlowe-dashboard.generated-purescript --no-link --json | jq -r .[0].outputs.out) $generated
    chmod -R +w $generated
  '';

  start-backend = pkgs.writeShellScriptBin "marlowe-run-server" ''
    echo "marlowe-pab-server: for development use only"
    export NOMAD_PORT_wbe="''${NOMAD_PORT_wbe:-8090}"
    export NOMAD_PORT_index="''${NOMAD_PORT_index:-9083}"
    cat > marlowe-run.json <<EOF
    {
      "wbeConfig": { "host": "localhost", "port": $NOMAD_PORT_wbe },
      "chainIndexConfig": { "host": "localhost", "port": $MONAD_PORT_index },
      "staticPath": "/var/empty",
      "verbosity": 3
    }
    EOF
    (trap 'kill 0' SIGINT;
      nix run ../.#marlowe-dashboard.marlowe-invoker -- --config plutus-pab.yaml webserver &
      nix run ../.#marlowe-dashboard.marlowe-run-backend-invoker -- webserver -c ./marlowe-run.json -n 1564
    )
  '';

  spagoBuild =
    ''spago build --purs-args "--strict --stash --censor-lib --stash --is-lib=generated --is-lib=.spago"'';

  build-client = writeShellScriptBinInRepoRoot "marlowe-run-spago" ''
    cd marlowe-dashboard-client
    ${spagoBuild}
  '';

  test-client = writeShellScriptBinInRepoRoot "marlowe-run-test" ''
    cd marlowe-dashboard-client
    ${spagoBuild}
    npm test
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
        ${pkgs.nodejs}/bin/npm run test
      '';
      name = "marlowe-dashboard-client";
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
  inherit
    client marlowe-invoker marlowe-run-backend-invoker generate-purescript
    generated-purescript start-backend build-client test-client;
}
