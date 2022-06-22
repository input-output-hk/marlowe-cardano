{ marlowe-playground, marlowe-pab, web-ghc, marlowe-dashboard, marlowe-dashboard-server, cardano-node, cardano-wallet, plutus-chain-index, docs, pkgs, sources }:
let
  staticSite = pkgs.callPackage (sources.plutus-apps + "/bitte/static-site.nix") { };
  playgroundStatic = pkgs.callPackage (sources.plutus-apps + "/bitte/playground-static.nix") { inherit staticSite; docs = docs.site; };

  inherit (pkgs) wait-for-socket;

  sleep-until-restart-slot = pkgs.writeShellScriptBin "sleep-until-restart-slot" ''
    set -eEuo pipefail

    export PATH="${pkgs.coreutils}/bin"

    nowHour=$(date -u +%-H)
    hoursLeft=$((3 - (nowHour % 3)))
    wakeHour=$(((nowHour + hoursLeft) % 24))
    exec sleep $((($(date -u -f - +%s- <<< "$wakeHour"$':00 tomorrow\nnow')0)%86400))
  '';

  run-entrypoints = network: {
    marlowe-run-entrypoint = pkgs.callPackage ./pab.nix {
      pabExe = "${marlowe-pab}/bin/marlowe-pab";
      staticPkg = marlowe-dashboard.client;
      inherit wait-for-socket network sleep-until-restart-slot;
    };

    marlowe-run-server-entrypoint = pkgs.callPackage ./marlowe-run-server.nix {
      marlowe-dashboard-server = marlowe-dashboard-server;
      inherit network;
    };

    node = pkgs.callPackage ./node {
      inherit cardano-node network;
    };

    wbe = pkgs.callPackage ./wbe.nix {
      inherit cardano-wallet wait-for-socket network sleep-until-restart-slot;
    };

    chain-index = pkgs.callPackage ./chain-index.nix { inherit plutus-chain-index wait-for-socket network; };
  };

  runs = builtins.listToAttrs (map
    (name: {
      inherit name;
      value = run-entrypoints pkgs.networks.${name};
    }) [ "testnet-dev" "testnet-pioneers" ]);
in
{
  web-ghc-server-entrypoint = pkgs.callPackage (sources.plutus-apps + "/bitte/web-ghc-server.nix") {
    web-ghc-server = web-ghc;
  };

  marlowe-playground-server-entrypoint = pkgs.callPackage (sources.plutus-apps + "/bitte/plutus-playground-server.nix") {
    variant = "marlowe";
    pkg = marlowe-playground.server;
  };
  marlowe-playground-client-entrypoint = playgroundStatic {
    client = marlowe-playground.client;
    variant = "marlowe";
  };
  node-socat = pkgs.callPackage ./node-socat.nix {
    inherit wait-for-socket;
  };
} // runs.testnet-dev // runs
