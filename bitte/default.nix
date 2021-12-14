{ marlowe-playground, marlowe-pab, web-ghc, marlowe-dashboard, cardano-node, cardano-wallet, plutus-chain-index, docs, pkgs, sources }:
let
  staticSite = pkgs.callPackage (sources.plutus-apps + "/bitte/static-site.nix") { };
  playgroundStatic = pkgs.callPackage (sources.plutus-apps + "/bitte/playground-static.nix") { inherit staticSite; docs = docs.site; };
  wait-for-socket = pkgs.writeShellScriptBin "wait-for-socket" ''
    set -eEuo pipefail

    export PATH="${pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.socat ]}"

    sock_path="$1"
    delay_iterations="''${2:-8}"

    for ((i=0;i<delay_iterations;i++))
    do
      if socat -u OPEN:/dev/null "UNIX-CONNECT:''${sock_path}"
      then
        exit 0
      fi
      let delay=2**i
      echo "Connecting to ''${sock_path} failed, sleeping for ''${delay} seconds" >&2
      sleep "''${delay}"
    done

    socat -u OPEN:/dev/null "UNIX-CONNECT:''${sock_path}"
  '';
in
{
  web-ghc-server-entrypoint = pkgs.callPackage ./web-ghc-server.nix {
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

  marlowe-run-entrypoint = pkgs.callPackage ./pab.nix {
    pabExe = "${marlowe-pab}/bin/marlowe-pab";
    staticPkg = marlowe-dashboard.client;
    inherit wait-for-socket;
  };

  node = pkgs.callPackage ./node {
    inherit cardano-node;
  };

  wbe = pkgs.callPackage ./wbe.nix { inherit cardano-wallet wait-for-socket; };

  chain-index = pkgs.callPackage ./chain-index.nix { inherit plutus-chain-index wait-for-socket; };
}
