# Needed variables:
#   NOMAD_PORT_run
#   NOMAD_IP_wbe
#   NOMAD_PORT_wbe
{ writeShellScriptBin, marlowe-dashboard-server, coreutils, lib }:
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH="${lib.makeBinPath [ coreutils marlowe-dashboard-server ]}"

  cat > marlowe-run.json <<EOF
  {
    "wbeConfig": { "host": "$NOMAD_IP_wbe", "port": $NOMAD_PORT_wbe },
    "staticPath": "/var/empty"
  }
  EOF

  exec marlowe-dashboard-server webserver --bind 0.0.0.0 --port $NOMAD_PORT_run --config marlowe-run.json
''
