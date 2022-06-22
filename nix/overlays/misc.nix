self: _:
let
  inherit (self) writeShellScriptBin lib coreutils socat;
in
{
  wait-for-socket = writeShellScriptBin "wait-for-socket" ''
    set -eEuo pipefail

    export PATH="${lib.makeBinPath [ coreutils socat ]}"

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
}
