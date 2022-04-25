# This is meant to be called from:
#  * nix/default.nix
{ pkgs
, checkMaterialization
, system ? builtins.currentSystem
, config ? { allowUnfreePredicate = (import ../lib/unfree.nix).unfreePredicate; }
, sources
, enableHaskellProfiling
, source-repo-override
}:
let
  inherit (pkgs) stdenv;

  gitignore-nix = pkgs.callPackage sources.gitignore-nix { };

  # { index-state, compiler-nix-name, project, projectPackages, packages, extraPackages }
  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources;
    inherit checkMaterialization enableHaskellProfiling;
    inherit source-repo-override;
    inherit (sources) actus-tests;

    # This ensures that the utility scripts produced in here will run on the current system, not
    # the build system, so we can run e.g. the darwin ones on linux
    inherit (pkgs.evalPackages) writeShellScript;
  };

  #
  # additional haskell packages from ./nix/pkgs/haskell-extra
  #
  exeFromExtras = x: haskell.extraPackages."${x}".components.exes."${x}";
  cabal-install = haskell.extraPackages.cabal-install.components.exes.cabal;
  cardano-repo-tool = exeFromExtras "cardano-repo-tool";
  stylish-haskell = exeFromExtras "stylish-haskell";
  hlint = exeFromExtras "hlint";
  haskell-language-server = exeFromExtras "haskell-language-server";
  haskell-language-server-wrapper = pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''${haskell-language-server}/bin/haskell-language-server "$@"'';
  hie-bios = exeFromExtras "hie-bios";

  # These are needed to pull the cardano-cli and cardano-node in the nix-shell.
  inherit (haskell.project.hsPkgs.cardano-cli.components.exes) cardano-cli;
  inherit (haskell.project.hsPkgs.cardano-node.components.exes) cardano-node;
  # HACK WARNING:
  # plutus-chain-index should be the following commented line, but that is currently causing
  # a baffling segfault, instead I'm just using the string `plutus-chain-index` because nix-shell
  # is already putting it on the $PATH
  # inherit (haskell.project.hsPkgs.plutus-chain-index.components.exes) plutus-chain-index;
  plutus-chain-index = "plutus-chain-index";
  # same hack warning:
  # inherit (haskell.project.hsPkgs.marlowe.components.exes) marlowe-pab;
  marlowe-pab = "marlowe-pab";
  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';

  fixStylishHaskell = pkgs.callPackage (sources.plutus-apps + "/nix/pkgs/fix-stylish-haskell") { inherit stylish-haskell; };
  fixPngOptimization = pkgs.callPackage (sources.plutus-apps + "/nix/pkgs/fix-png-optimization") { };
  updateMaterialized = writeShellScriptBinInRepoRoot "updateMaterialized" ''
    # This runs the 'updateMaterialize' script in all platform combinations we care about.
    # See the comment in ./haskell/haskell.nix

    # Update the linux files (will do for all unixes atm).
    $(nix-build default.nix -A marlowe.haskell.project.plan-nix.passthru.updateMaterialized --argstr system x86_64-linux "$@") &
    $(nix-build default.nix -A marlowe.haskell.project.plan-nix.passthru.updateMaterialized --argstr system x86_64-darwin "$@") &
    $(nix-build default.nix -A marlowe.haskell.project.plan-nix.passthru.updateMaterialized --argstr system windows "$@") &
    $(nix-build default.nix -A marlowe.haskell.project.projectCross.mingwW64.plan-nix.passthru.updateMaterialized --argstr system x86_64-linux "$@") &

    # This updates the sha files for the extra packages
    $(nix-build default.nix -A marlowe.haskell.extraPackages.updateAllShaFiles --argstr system x86_64-linux "$@") &
    $(nix-build default.nix -A marlowe.haskell.extraPackages.updateAllShaFiles --argstr system x86_64-darwin "$@") &
    wait
  '';

  network = pkgs.networks.testnet-dev;

  devNetworkConfig = rec {
    node = {
      config-file = pkgs.writeTextFile {
        name = "node-config.json";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/node-config.nix { config = network.nodeConfig; });
      };
      port = 3001;
      socket-path = "ipc/node.socket";
      database-path = "db/node.db";
    };
    wallet = {
      testnet = network.nodeConfig.ByronGenesisFile;
      database-path = "db/wallet.db";
      port = 8090;
    };
    chain-index = {
      network-id = network.magic;
      database-path = "db/chain-index.db";
      port = 9083;
    };
    pab = {
      database-path = "db/pab.db";
      port = 9080;
      config-params = {
        dbConfigFile = pab.database-path + "/marlowe-pab.db";
        baseUrl = "http://localhost:${toString pab.port}";
        walletUrl = "http://localhost:${toString wallet.port}";
        socket-path = node.socket-path;
        inherit network;
        protocol-parameters = "ipc/testnet.protocol";
      };
      config-file = pkgs.writeTextFile {
        name = "marlowe-pab.yaml";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/pab-config.nix pab.config-params);
      };
    };
    topology = network.topology;
  };

  start-cardano-node = writeShellScriptBinInRepoRoot "start-cardano-node" ''
    mkdir -p ${devNetworkConfig.node.database-path}
    mkdir ipc
    cardano-node run \
            --config ${devNetworkConfig.node.config-file} \
            --topology ${devNetworkConfig.topology} \
            --port ${toString devNetworkConfig.node.port} \
            --socket-path ${devNetworkConfig.node.socket-path} \
            --database-path ${devNetworkConfig.node.database-path}
  '';

  start-wallet = writeShellScriptBinInRepoRoot "start-cardano-wallet" ''
    mkdir -p ${devNetworkConfig.wallet.database-path}

    cardano-wallet serve \
      --testnet ${devNetworkConfig.wallet.testnet} \
      --database ${devNetworkConfig.wallet.database-path} \
      --node-socket ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.wallet.port}
  '';

  start-chain-index = writeShellScriptBinInRepoRoot "start-chain-index" ''
    mkdir -p ${devNetworkConfig.chain-index.database-path}

    ${plutus-chain-index} start-index \
      --network-id ${toString devNetworkConfig.chain-index.network-id} \
      --db-path ${devNetworkConfig.chain-index.database-path}/ci.sqlite \
      --socket-path ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.chain-index.port}

  '';

  start-marlowe-pab = writeShellScriptBinInRepoRoot "start-marlowe-pab" ''
    mkdir -p ${devNetworkConfig.pab.database-path}
    echo "before"
    [ ! -f ${devNetworkConfig.pab.config-params.dbConfigFile}/marlowe-pab.db ] && \
      ${marlowe-pab} migrate --config ${devNetworkConfig.pab.config-file}
    echo ${devNetworkConfig.pab.config-file}
    echo "after"
  '';

  updateClientDeps = pkgs.callPackage ./update-client-deps.nix {
    inherit purs spago spago2nix writeShellScriptBinInRepoRoot;
  };

  #
  # sphinx python packages
  #
  sphinx-markdown-tables = pkgs.python3Packages.callPackage (sources.plutus-apps + "/nix/pkgs/sphinx-markdown-tables") { };
  sphinxemoji = pkgs.python3Packages.callPackage (sources.plutus-apps + "/nix/pkgs/sphinxemoji") { };

  # By default pre-commit-hooks.nix uses its own pinned version of nixpkgs. In order to
  # to get it to use our version we have to (somewhat awkwardly) use `nix/default.nix`
  # to which both `nixpkgs` and `system` can be passed.
  nix-pre-commit-hooks = (pkgs.callPackage (sources.pre-commit-hooks-nix + "/nix/default.nix") {
    inherit system;
    inherit (sources) nixpkgs;
  });

  # easy-purescript-nix has some kind of wacky internal IFD
  # usage that breaks the logic that makes source fetchers
  # use native dependencies. This isn't easy to fix, since
  # the only places that need to use native dependencies
  # are deep inside, and we don't want to build the whole
  # thing native. Fortunately, we only want to build the
  # client on Linux, so that's okay. However, it does
  # mean that e.g. we can't build the client dep updating
  # script on Darwin.
  easyPS = pkgs.callPackage (sources.easy-purescript-nix) { };

  # We pull out some packages from easyPS that are a pain to get otherwise.
  # This does mean we can't as easily control the version we get, though.
  inherit (easyPS) purs-tidy purs spago spago2nix psa purescript-language-server pscid;

  # sphinx haddock support
  sphinxcontrib-haddock = pkgs.callPackage (sources.sphinxcontrib-haddock) { pythonPackages = pkgs.python3Packages; };

  # ghc web service
  web-ghc = pkgs.callPackage (sources.plutus-apps + "/nix/pkgs/web-ghc") { inherit haskell; extraPackagesFun = ps: [ ps.marlowe ]; };

  webCommon = pkgs.callPackage sources.web-common { inherit gitignore-nix; };

  formatting = pkgs.callPackage ./formatting.nix {
    inherit easyPS writeShellScriptBinInRepoRoot;
  };


  # combined haddock documentation for all public plutus libraries
  plutus-haddock-combined =
    let
      haddock-combine = pkgs.callPackage (sources.plutus-core + "/nix/lib/haddock-combine.nix") {
        ghc = haskell.projectAllHaddock.pkg-set.config.ghc.package;
        inherit (sphinxcontrib-haddock) sphinxcontrib-haddock;
      };
    in
    pkgs.callPackage (sources.plutus-core + "/nix/pkgs/plutus-haddock-combined") {
      inherit haskell haddock-combine;
      inherit (pkgs) haskell-nix;
    };

  # Collect everything to be exported under `plutus.lib`: builders/functions/utils
  lib = rec {
    inherit gitignore-nix;
    haddock-combine = pkgs.callPackage (sources.plutus-core + "/nix/lib/haddock-combine.nix") { inherit sphinxcontrib-haddock; };
    filterNpm = pkgs.callPackage (sources.plutus-apps + "/nix/lib/filter-npm.nix") { };
    npmlock2nix = pkgs.callPackage sources.npmlock2nix { };
    buildPursPackage = pkgs.callPackage (sources.plutus-apps + "/nix/lib/purescript.nix") { inherit easyPS; inherit (pkgs) nodejs; };
    buildNodeModules = pkgs.callPackage (sources.plutus-apps + "/nix/lib/node_modules.nix") ({
      inherit npmlock2nix;
    } // pkgs.lib.optionalAttrs (stdenv.isDarwin) {
      CoreServices = pkgs.darwin.apple_sdk.frameworks.CoreServices;
      xcodebuild = pkgs.xcodebuild;
    });

  };

in
{
  inherit sphinx-markdown-tables sphinxemoji sphinxcontrib-haddock;
  inherit nix-pre-commit-hooks;
  inherit haskell cabal-install cardano-repo-tool stylish-haskell hlint haskell-language-server haskell-language-server-wrapper hie-bios cardano-cli cardano-node;
  inherit purs-tidy purs spago spago2nix psa purescript-language-server pscid;
  inherit fixStylishHaskell fixPngOptimization updateMaterialized updateClientDeps writeShellScriptBinInRepoRoot;
  inherit web-ghc;
  inherit easyPS plutus-haddock-combined;
  inherit lib;
  inherit webCommon;
  inherit start-cardano-node start-wallet start-chain-index start-marlowe-pab;
  inherit (formatting) fix-prettier fix-purs-tidy fix-dhall purs-tidy-hook dhall-hook;
}
