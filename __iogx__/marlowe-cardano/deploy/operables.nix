{ inputs, pkgs }:
let
  inherit (inputs) self std bitte-cells;
  inherit (pkgs)
    lib
    jq
    sqitchPg
    postgresql
    coreutils
    writeShellScriptBin
    socat
    netcat
    curl
    ;
  inherit (bitte-cells._utils.packages) srvaddr;

  marlowe-chain-indexer = self.packages.ghc8107-marlowe-chain-sync-exe-marlowe-chain-indexer;
  marlowe-chain-sync = self.packages.ghc8107-marlowe-chain-sync-exe-marlowe-chain-sync;
  marlowe-tx = self.packages.ghc8107-marlowe-runtime-exe-marlowe-tx;
  marlowe-proxy = self.packages.ghc8107-marlowe-runtime-exe-marlowe-proxy;
  marlowe-indexer = self.packages.ghc8107-marlowe-runtime-exe-marlowe-indexer;
  marlowe-web-server = self.packages.ghc8107-marlowe-runtime-web-exe-marlowe-web-server;
  marlowe-sync = self.packages.ghc8107-marlowe-runtime-exe-marlowe-sync;

  # Ensure this path only changes when sqitch.plan file is updated, or DDL
  # files are updated.
  chain-sync-sqitch-plan-dir = (builtins.path {
    path = self;
    name = "marlowe-chain-sync-sqitch-plan";
    filter = path: type:
      path == "${self}/marlowe-chain-sync"
        || path == "${self}/marlowe-chain-sync/sqitch.plan"
        || lib.hasPrefix "${self}/marlowe-chain-sync/deploy" path
        || lib.hasPrefix "${self}/marlowe-chain-sync/revert" path;
  }) + "/marlowe-chain-sync";

  # Ensure this path only changes when sqitch.plan file is updated, or DDL
  # files are updated.
  runtime-sqitch-plan-dir = (builtins.path {
    path = self;
    name = "marlowe-runtime-sqitch-plan";
    filter = path: type:
      path == "${self}/marlowe-runtime"
        || path == "${self}/marlowe-runtime/marlowe-indexer"
        || path == "${self}/marlowe-runtime/marlowe-indexer/sqitch.plan"
        || lib.hasPrefix "${self}/marlowe-runtime/marlowe-indexer/deploy" path
        || lib.hasPrefix "${self}/marlowe-runtime/marlowe-indexer/revert" path;
  }) + "/marlowe-runtime/marlowe-indexer";

  database-uri = "postgresql://$DB_USER:$DB_PASS@$DB_HOST/$DB_NAME";

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

  wait-for-tcp = writeShellScriptBin "wait-for-tcp" ''
    set -eEuo pipefail

    export PATH="${lib.makeBinPath [ coreutils netcat ]}"

    ip="$1"
    port="$2"
    delay_iterations="''${3:-8}"

    for ((i=0;i<delay_iterations;i++))
    do
      if nc -v -w 2 -z "$ip" "$port"
      then
        exit 0
      fi
      let delay=2**i
      echo "Connecting to ''${ip}:''${port} failed, sleeping for ''${delay} seconds" >&2
      sleep "''${delay}"
    done

    nc -v -w 2 -z "$ip" "$port"
  '';

  inherit (std.lib.ops) mkOperable;

  probes = {
    livenessProbe = std.lib.ops.writeScript {
      name = "liveness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$HTTP_PORT/live"
      '';
    };
    readinessProbe = std.lib.ops.writeScript {
      name = "readiness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$HTTP_PORT/ready"
      '';
    };
  };

  mkOperableWithProbes = args: mkOperable (args // probes);

in
{
  marlowe-chain-indexer = mkOperableWithProbes {
    package = marlowe-chain-indexer;
    runtimeInputs = [ jq sqitchPg srvaddr postgresql coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # NODE_CONFIG: path to the cardano node config file
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS,
      # HTTP_PORT: port number for the HTTP healthcheck server
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      DATABASE_URI=${database-uri}
      cd ${chain-sync-sqitch-plan-dir}
      mkdir -p /tmp
      HOME="$(mktemp -d)" # Ensure HOME is writable for sqitch config
      export TZ=Etc/UTC
      sqitch config --user user.name chainindexer
      sqitch config --user user.email example@example.com
      sqitch --quiet deploy --target "$DATABASE_URI"
      cd -

      NODE_CONFIG_DIR=$(dirname "$NODE_CONFIG")
      BYRON_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ByronGenesisFile "$NODE_CONFIG")"
      SHELLEY_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ShelleyGenesisFile "$NODE_CONFIG")"
      BYRON_GENESIS_HASH=$(jq -r '.ByronGenesisHash' "$NODE_CONFIG")
      CARDANO_TESTNET_MAGIC=$(jq -r '.networkMagic' "$SHELLEY_GENESIS_CONFIG");
      export CARDANO_TESTNET_MAGIC

      ${wait-for-socket}/bin/wait-for-socket "$CARDANO_NODE_SOCKET_PATH"

      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-chain-indexer}/bin/marlowe-chain-indexer \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --database-uri  "$DATABASE_URI" \
          --shelley-genesis-config-file "$SHELLEY_GENESIS_CONFIG" \
          --genesis-config-file "$BYRON_GENESIS_CONFIG" \
          --genesis-config-file-hash "$BYRON_GENESIS_HASH" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-chain-indexer}/bin/marlowe-chain-indexer \
          --log-config-file "$LOG_CONFIG_FILE" \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --database-uri  "$DATABASE_URI" \
          --shelley-genesis-config-file "$SHELLEY_GENESIS_CONFIG" \
          --genesis-config-file "$BYRON_GENESIS_CONFIG" \
          --genesis-config-file-hash "$BYRON_GENESIS_HASH" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-chain-sync = mkOperableWithProbes {
    package = marlowe-chain-sync;
    runtimeInputs = [ srvaddr jq coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, QUERY_PORT, JOB_PORT: network binding
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS,
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${QUERY_PORT:-}" ] && echo "QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${JOB_PORT:-}" ] && echo "JOB_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi
      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      NODE_CONFIG_DIR=$(dirname "$NODE_CONFIG")
      SHELLEY_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ShelleyGenesisFile "$NODE_CONFIG")"
      CARDANO_TESTNET_MAGIC=$(jq -r '.networkMagic' "$SHELLEY_GENESIS_CONFIG");
      export CARDANO_TESTNET_MAGIC

      ${wait-for-socket}/bin/wait-for-socket "$CARDANO_NODE_SOCKET_PATH"

      DATABASE_URI=${database-uri}
      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-chain-sync}/bin/marlowe-chain-sync \
          --host "$HOST" \
          --port "$PORT" \
          --query-port "$QUERY_PORT" \
          --job-port "$JOB_PORT" \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --database-uri  "$DATABASE_URI" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-chain-sync}/bin/marlowe-chain-sync \
          --log-config-file "$LOG_CONFIG_FILE" \
          --host "$HOST" \
          --port "$PORT" \
          --query-port "$QUERY_PORT" \
          --job-port "$JOB_PORT" \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --database-uri  "$DATABASE_URI" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-indexer = mkOperableWithProbes {
    package = marlowe-indexer;
    runtimeInputs = [ sqitchPg srvaddr postgresql coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_PORT, MARLOWE_CHAIN_SYNC_QUERY_PORT: connection info to marlowe-chain-sync
      # DB_NAME, DB_USER, DB_PASS,
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      DATABASE_URI=${database-uri}
      cd ${runtime-sqitch-plan-dir}
      mkdir -p /tmp
      HOME="$(mktemp -d)" # Ensure HOME is writable for sqitch config
      export TZ=Etc/UTC
      sqitch config --user user.name marloweindexer
      sqitch config --user user.email example@example.com
      sqitch --quiet deploy --target "$DATABASE_URI"
      cd -

      ${wait-for-tcp}/bin/wait-for-tcp "$MARLOWE_CHAIN_SYNC_HOST" "$MARLOWE_CHAIN_SYNC_PORT"

      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-indexer}/bin/marlowe-indexer \
          --database-uri  "$DATABASE_URI" \
          --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
          --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
          --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-indexer}/bin/marlowe-indexer \
          --log-config-file "$LOG_CONFIG_FILE" \
          --database-uri  "$DATABASE_URI" \
          --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
          --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
          --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-sync = mkOperableWithProbes {
    package = marlowe-sync;
    runtimeInputs = [ srvaddr coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, MARLOWE_SYNC_PORT, MARLOWE_HEADER_SYNC_PORT, MARLOWE_QUERY_PORT: network binding
      # DB_NAME, DB_USER, DB_PASS,
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_SYNC_PORT:-}" ] && echo "MARLOWE_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_HEADER_SYNC_PORT:-}" ] && echo "MARLOWE_HEADER_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_QUERY_PORT:-}" ] && echo "MARLOWE_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      DATABASE_URI=${database-uri}
      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-sync}/bin/marlowe-sync \
          --database-uri  "$DATABASE_URI" \
          --host "$HOST" \
          --sync-port "$MARLOWE_SYNC_PORT" \
          --header-sync-port "$MARLOWE_HEADER_SYNC_PORT" \
          --query-port "$MARLOWE_QUERY_PORT" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-sync}/bin/marlowe-sync \
          --log-config-file "$LOG_CONFIG_FILE" \
          --database-uri  "$DATABASE_URI" \
          --host "$HOST" \
          --sync-port "$MARLOWE_SYNC_PORT" \
          --header-sync-port "$MARLOWE_HEADER_SYNC_PORT" \
          --query-port "$MARLOWE_QUERY_PORT" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-tx = mkOperableWithProbes {
    package = marlowe-tx;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT: network binding
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_PORT, MARLOWE_CHAIN_SYNC_QUERY_PORT, MARLOWE_CHAIN_SYNC_COMMAND_PORT: connection info to marlowe-chain-sync
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_COMMAND_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_COMMAND_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$MARLOWE_CHAIN_SYNC_HOST" "$MARLOWE_CHAIN_SYNC_PORT"

      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-tx}/bin/marlowe-tx \
          --host "$HOST" \
          --command-port "$PORT" \
          --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
          --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
          --chain-sync-command-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
          --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-tx}/bin/marlowe-tx \
          --log-config-file "$LOG_CONFIG_FILE" \
          --host "$HOST" \
          --command-port "$PORT" \
          --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
          --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
          --chain-sync-command-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
          --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-proxy = mkOperableWithProbes {
    package = marlowe-proxy;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT: network binding
      # TX_HOST, TX_PORT: connection info to marlowe-tx
      # SYNC_HOST, MARLOWE_SYNC_PORT, MARLOWE_HEADER_SYNC_PORT, MARLOWE_QUERY_PORT: connection info to marlowe-sync
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # LOG_CONFIG_FILE: The path to the JSON logging config file

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${TX_HOST:-}" ] && echo "TX_HOST env var must be set -- aborting" && exit 1
      [ -z "''${TX_PORT:-}" ] && echo "TX_PORT env var must be set -- aborting" && exit 1
      [ -z "''${SYNC_HOST:-}" ] && echo "SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_SYNC_PORT:-}" ] && echo "MARLOWE_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_HEADER_SYNC_PORT:-}" ] && echo "MARLOWE_HEADER_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_QUERY_PORT:-}" ] && echo "MARLOWE_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$TX_HOST" "$TX_PORT"
      ${wait-for-tcp}/bin/wait-for-tcp "$SYNC_HOST" "$MARLOWE_QUERY_PORT"

      if [ -z "''${LOG_CONFIG_FILE:-}" ]
      then
        ${marlowe-proxy}/bin/marlowe-proxy \
          --host "$HOST" \
          --port "$PORT" \
          --marlowe-sync-host "$SYNC_HOST" \
          --marlowe-sync-port "$MARLOWE_SYNC_PORT" \
          --marlowe-header-port "$MARLOWE_HEADER_SYNC_PORT" \
          --marlowe-query-port "$MARLOWE_QUERY_PORT" \
          --tx-host "$TX_HOST" \
          --tx-command-port "$TX_PORT" \
          --http-port "$HTTP_PORT"
      else
        ${marlowe-proxy}/bin/marlowe-proxy \
          --log-config-file "$LOG_CONFIG_FILE" \
          --host "$HOST" \
          --port "$PORT" \
          --marlowe-sync-host "$SYNC_HOST" \
          --marlowe-sync-port "$MARLOWE_SYNC_PORT" \
          --marlowe-header-port "$MARLOWE_HEADER_SYNC_PORT" \
          --marlowe-query-port "$MARLOWE_QUERY_PORT" \
          --tx-host "$TX_HOST" \
          --tx-command-port "$TX_PORT" \
          --http-port "$HTTP_PORT"
      fi
    '';
  };

  marlowe-web-server = mkOperable {
    package = marlowe-web-server;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # PORT: network binding
      # RUNTIME_HOST, RUNTIME_PORT: connection info to marlowe-proxy

      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${RUNTIME_HOST:-}" ] && echo "RUNTIME_HOST env var must be set -- aborting" && exit 1
      [ -z "''${RUNTIME_PORT:-}" ] && echo "RUNTIME_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$RUNTIME_HOST" "$RUNTIME_PORT"

      ${marlowe-web-server}/bin/marlowe-web-server \
        --http-port "$PORT" \
        --marlowe-runtime-host "$RUNTIME_HOST" \
        --marlowe-runtime-port "$RUNTIME_PORT" \
        --enable-open-api \
        --access-control-allow-origin-all
    '';
    livenessProbe = std.lib.ops.writeScript {
      name = "liveness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$PORT/healthcheck"
      '';
    };
    readinessProbe = std.lib.ops.writeScript {
      name = "readiness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$PORT/healthcheck"
      '';
    };
  };
}
