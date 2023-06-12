{ inputs, pkgs }:
let
  inherit (inputs.cardano-world.cardano.packages) cardano-node;

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

  sleep-until-restart-slot = pkgs.writeShellScriptBin "sleep-until-restart-slot" ''
    set -eEuo pipefail

    export PATH="${pkgs.coreutils}/bin"

    nowHour=$(date -u +%-H)
    hoursLeft=$((3 - (nowHour % 3)))
    wakeHour=$(((nowHour + hoursLeft) % 24))
    exec sleep $((($(date -u -f - +%s- <<< "$wakeHour"$':00 tomorrow\nnow')0)%86400))
  '';

  run-entrypoints = network: {
    node = pkgs.callPackage ./node {
      inherit cardano-node network;
    };

  };

  runs = builtins.listToAttrs (map
    (name: {
      inherit name;
      value = run-entrypoints inputs.self.networks.${name};
    }) [ "testnet-dev" "testnet-pioneers" "preprod" ]);
in
{
  node-socat = pkgs.callPackage ./node-socat.nix {
    inherit wait-for-socket;
  };
} // runs.testnet-dev // runs // { inherit cardano-node; }
