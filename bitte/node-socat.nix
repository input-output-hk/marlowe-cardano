{ writeShellScriptBin, wait-for-socket, socat, lib, coreutils }:

writeShellScriptBin "node-socat" ''
  set -eEuo pipefail

  export PATH=${lib.makeBinPath [ wait-for-socket socat coreutils ]}

  wait-for-socket ''${NOMAD_ALLOC_DIR}/node.sock

  exec socat TCP-LISTEN:''${NOMAD_PORT_node_socat},bind=''${NOMAD_IP_node_socat} UNIX-CONNECT:''${NOMAD_ALLOC_DIR}/node.sock
''
