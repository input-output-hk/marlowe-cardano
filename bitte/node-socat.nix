{ writeShellScriptBin, wait-for-socket, socat, lib, coreutils }:

writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH=${lib.makeBinPath [ wait-for-socket socat coreutils ]}

  wait-for-socket ''${NOMAD_ALLOC_DIR}/node.sock

  exec socat OPENSSL-LISTEN:''${NOMAD_PORT_node_socat},bind=''${NOMAD_IP_node_socat},reuseaddr,cert=''${SOCAT_SERVER_CERT},cafile=''${SOCAT_CLIENT_CERT},fork UNIX-CONNECT:''${NOMAD_ALLOC_DIR}/node.sock
''
