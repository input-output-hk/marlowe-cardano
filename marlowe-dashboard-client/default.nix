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
    cp -a $(nix-build -A marlowe-dashboard.generated-purescript --no-out-link) $generated
    chmod -R +w $generated
  '';

  start-backend = pkgs.writeShellScriptBin "marlowe-run-server" ''
    echo "marlowe-pab-server: for development use only"
    export NOMAD_PORT_wbe="''${NOMAD_PORT_wbe:-8090}"
    cat > marlowe-run.json <<EOF
    {
      "wbeConfig": { "host": "localhost", "port": $NOMAD_PORT_wbe },
      "staticPath": "/var/empty"
    }
    EOF
    (trap 'kill 0' SIGINT;
      $(nix-build ../default.nix --quiet --no-build-output -A marlowe-dashboard.marlowe-invoker)/bin/marlowe-pab --config plutus-pab.yaml webserver &
      $(nix-build ../default.nix -A marlowe-dashboard.marlowe-run-backend-invoker)/bin/marlowe-dashboard-server webserver -c ./marlowe-run.json
    )
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
      };
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
      WEB_COMMON_MARLOWE_SRC = webCommonMarlowe;
    });
in
{
  inherit client marlowe-invoker marlowe-run-backend-invoker generate-purescript generated-purescript start-backend;
}
