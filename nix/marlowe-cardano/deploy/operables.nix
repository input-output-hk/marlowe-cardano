{ inputs, pkgs, lib, ... }:

let
  inherit (inputs) self std marlowe-plutus;
  inherit (pkgs)
    jq
    sqitchPg
    postgresql
    coreutils
    writeShellScriptBin
    socat
    netcat
    curl
    z3
    ;

  inherit (self.packages)
    marlowe-chain-indexer
    marlowe-chain-sync
    marlowe-contract
    marlowe-runtime
    marlowe-tx
    marlowe-proxy
    marlowe-indexer
    marlowe-web-server
    marlowe-sync
    marlowe-pipe
    marlowe-scaling
    marlowe-oracle
    marlowe-finder;

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

  mkMarloweAppScript = script: ''
    #################
    # REQUIRED VARS #
    #################
    # MARLOWE_RT_HOST: Host name of the marlowe runtime server.

    [ -z "''${MARLOWE_RT_HOST:-}" ] && echo "MARLOWE_RT_HOST env var must be set -- aborting" && exit 1

    #################
    # OPTIONAL VARS #
    #################
    # MARLOWE_RT_PORT: Port number of the marlowe runtime server.
    # TIMEOUT_SECONDS: Timeout in seconds for transaction confirmation.
    # BUILD_SECONDS: Wait specified seconds before transaction construction.
    # CONFIRM_SECONDS: Wait specified seconds after transaction confirmation.
    # RETRY_SECONDS: Wait specified seconds after a failed transaction before trying again.
    # RETRY_LIMIT: Maximum number of attempts for trying a failed transaction again.

    export MARLOWE_RT_PORT="''${MARLOWE_RT_PORT:-3700}"
    export TIMEOUT_SECONDS="''${TIMEOUT_SECONDS:-900}"
    export BUILD_SECONDS="''${BUILD_SECONDS:-3}"
    export CONFIRM_SECONDS="''${CONFIRM_SECONDS:-3}"
    export RETRY_SECONDS="''${RETRY_SECONDS:-10}"
    export RETRY_LIMIT="''${RETRY_LIMIT:-5}"

    ${wait-for-tcp}/bin/wait-for-tcp "$MARLOWE_RT_HOST" "$MARLOWE_RT_PORT"

    ${script}
  '';
in
{
  marlowe-chain-indexer = mkOperableWithProbes {
    package = marlowe-chain-indexer;
    runtimeInputs = [ jq sqitchPg postgresql coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # NODE_CONFIG: path to the cardano node config file
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS, DB_HOST,
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
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
      if [[ $(jq -r .networkId "$SHELLEY_GENESIS_CONFIG") != "Mainnet" ]]
      then
        CARDANO_TESTNET_MAGIC=$(jq -r '.networkMagic' "$SHELLEY_GENESIS_CONFIG");
        export CARDANO_TESTNET_MAGIC
      fi

      ${wait-for-socket}/bin/wait-for-socket "$CARDANO_NODE_SOCKET_PATH"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-chain-indexer}"

      ${marlowe-chain-indexer}/bin/marlowe-chain-indexer \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI" \
        --shelley-genesis-config-file "$SHELLEY_GENESIS_CONFIG" \
        --genesis-config-file "$BYRON_GENESIS_CONFIG" \
        --genesis-config-file-hash "$BYRON_GENESIS_HASH" \
        --http-port "$HTTP_PORT"
    '';
  };

  marlowe-chain-sync = mkOperableWithProbes {
    package = marlowe-chain-sync;
    runtimeInputs = [ jq coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, QUERY_PORT, JOB_PORT: network binding
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS, DB_HOST,
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${QUERY_PORT:-}" ] && echo "QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${JOB_PORT:-}" ] && echo "JOB_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      NODE_CONFIG_DIR=$(dirname "$NODE_CONFIG")
      SHELLEY_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ShelleyGenesisFile "$NODE_CONFIG")"
      if [[ $(jq -r .networkId "$SHELLEY_GENESIS_CONFIG") != "Mainnet" ]]
      then
        CARDANO_TESTNET_MAGIC=$(jq -r '.networkMagic' "$SHELLEY_GENESIS_CONFIG");
        export CARDANO_TESTNET_MAGIC
      fi

      ${wait-for-socket}/bin/wait-for-socket "$CARDANO_NODE_SOCKET_PATH"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-chain-sync}"

      DATABASE_URI=${database-uri}
      ${marlowe-chain-sync}/bin/marlowe-chain-sync \
        --host "$HOST" \
        --port "$PORT" \
        --query-port "$QUERY_PORT" \
        --job-port "$JOB_PORT" \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI" \
        --http-port "$HTTP_PORT"
    '';
  };

  marlowe-indexer = mkOperableWithProbes {
    package = marlowe-indexer;
    runtimeInputs = [ sqitchPg postgresql coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_PORT, MARLOWE_CHAIN_SYNC_QUERY_PORT: connection info to marlowe-chain-sync
      # DB_NAME, DB_USER, DB_PASS, DB_HOST,
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
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

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-indexer}"

      ${marlowe-indexer}/bin/marlowe-indexer \
        --database-uri  "$DATABASE_URI" \
        --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
        --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
        --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
        --http-port "$HTTP_PORT"
    '';
  };

  marlowe-sync = mkOperableWithProbes {
    package = marlowe-sync;
    runtimeInputs = [ coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_QUERY_PORT: connection info to marlowe-chain-sync
      # HOST, MARLOWE_SYNC_PORT, MARLOWE_BULK_SYNC_PORT,MARLOWE_HEADER_SYNC_PORT, MARLOWE_QUERY_PORT: network binding
      # DB_NAME, DB_USER, DB_PASS, DB_HOST,
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_SYNC_PORT:-}" ] && echo "MARLOWE_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_HEADER_SYNC_PORT:-}" ] && echo "MARLOWE_HEADER_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_BULK_SYNC_PORT:-}" ] && echo "MARLOWE_BULK_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_QUERY_PORT:-}" ] && echo "MARLOWE_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$MARLOWE_CHAIN_SYNC_HOST" "$MARLOWE_CHAIN_SYNC_QUERY_PORT"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-sync}"

      DATABASE_URI=${database-uri}
      ${marlowe-sync}/bin/marlowe-sync \
        --database-uri  "$DATABASE_URI" \
        --host "$HOST" \
        --sync-port "$MARLOWE_SYNC_PORT" \
        --header-sync-port "$MARLOWE_HEADER_SYNC_PORT" \
        --bulk-sync-port "$MARLOWE_BULK_SYNC_PORT" \
        --query-port "$MARLOWE_QUERY_PORT" \
        --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
        --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
        --http-port "$HTTP_PORT"
    '';
  };

  marlowe-tx = mkOperableWithProbes {
    package = marlowe-tx;
    runtimeInputs = [ z3 marlowe-plutus.packages.marlowe-minting-validator ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT: network binding
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_PORT, MARLOWE_CHAIN_SYNC_QUERY_PORT, MARLOWE_CHAIN_SYNC_COMMAND_PORT: connection info to marlowe-chain-sync
      # CONTRACT_HOST, CONTRACT_QUERY_PORT: connection info to marlowe-contract
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_COMMAND_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_COMMAND_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CONTRACT_HOST:-}" ] && echo "CONTRACT_HOST env var must be set -- aborting" && exit 1
      [ -z "''${CONTRACT_QUERY_PORT:-}" ] && echo "CONTRACT_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$MARLOWE_CHAIN_SYNC_HOST" "$MARLOWE_CHAIN_SYNC_PORT"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-tx}"

      ${marlowe-tx}/bin/marlowe-tx \
        --host "$HOST" \
        --command-port "$PORT" \
        --chain-sync-port "$MARLOWE_CHAIN_SYNC_PORT" \
        --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
        --chain-sync-command-port "$MARLOWE_CHAIN_SYNC_COMMAND_PORT" \
        --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
        --contract-host "$CONTRACT_HOST" \
        --contract-query-port "$CONTRACT_QUERY_PORT" \
        --http-port "$HTTP_PORT" \
        --minting-policy-cmd marlowe-minting-validator
    '';
  };

  marlowe-contract = mkOperableWithProbes {
    package = marlowe-contract;
    runtimeInputs = [ coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, QUERY_PORT, TRANSFER_PORT: network binding
      # MARLOWE_CHAIN_SYNC_HOST, MARLOWE_CHAIN_SYNC_QUERY_PORT: connection info to marlowe-chain-sync
      # SYNC_HOST, MARLOWE_BULK_SYNC_PORT, connection info to marlowe-sync
      # STORE_DIR: location of the contract store directory
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service
      # MAX_STORE_SIZE: the maximum size of the contract store, in bytes
      # MIN_CONTRACT_AGE: the minimum age of contracts in the store before they can be garbage collected, in seconds.

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${QUERY_PORT:-}" ] && echo "QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${TRANSFER_PORT:-}" ] && echo "TRANSFER_PORT env var must be set -- aborting" && exit 1
      [ -z "''${STORE_DIR:-}" ] && echo "STORE_DIR env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_HOST:-}" ] && echo "MARLOWE_CHAIN_SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_CHAIN_SYNC_QUERY_PORT:-}" ] && echo "MARLOWE_CHAIN_SYNC_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${SYNC_HOST:-}" ] && echo "SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_BULK_SYNC_PORT:-}" ] && echo "MARLOWE_BULK_SYNC_PORT env var must be set -- aborting" && exit 1

      mkdir -p /tmp /store

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-contract}"
      export MAX_STORE_SIZE="''${MAX_STORE_SIZE:-8589934592}" # 8 GB
      export MIN_CONTRACT_AGE="''${MIN_CONTRACT_AGE:-86400s}" # 1 day in seconds

      ${marlowe-contract}/bin/marlowe-contract \
        --host "$HOST" \
        --port "$PORT" \
        --query-port "$QUERY_PORT" \
        --transfer-port "$TRANSFER_PORT" \
        --chain-sync-host "$MARLOWE_CHAIN_SYNC_HOST" \
        --chain-sync-query-port "$MARLOWE_CHAIN_SYNC_QUERY_PORT" \
        --marlowe-sync-host "$SYNC_HOST" \
        --marlowe-bulk-port "$MARLOWE_BULK_SYNC_PORT" \
        --store-dir "$STORE_DIR" \
        --http-port "$HTTP_PORT" \
        --min-contract-age "$MIN_CONTRACT_AGE" \
        --max-store-size "$MAX_STORE_SIZE"
    '';
  };

  marlowe-proxy = mkOperableWithProbes {
    package = marlowe-proxy;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, TRACED_PORT: network binding
      # TX_HOST, TX_PORT: connection info to marlowe-tx
      # SYNC_HOST, MARLOWE_SYNC_PORT, MARLOWE_BULK_SYNC_PORT, MARLOWE_HEADER_SYNC_PORT, MARLOWE_QUERY_PORT: connection info to marlowe-sync
      # CONTRACT_HOST, LOAD_PORT, CONTRACT_QUERY_PORT, TRANSFER_PORT: connection info to marlowe-contract
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${TRACED_PORT:-}" ] && echo "TRACED_PORT env var must be set -- aborting" && exit 1
      [ -z "''${TX_HOST:-}" ] && echo "TX_HOST env var must be set -- aborting" && exit 1
      [ -z "''${TX_PORT:-}" ] && echo "TX_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CONTRACT_HOST:-}" ] && echo "CONTRACT_HOST env var must be set -- aborting" && exit 1
      [ -z "''${LOAD_PORT:-}" ] && echo "LOAD_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CONTRACT_QUERY_PORT:-}" ] && echo "CONTRACT_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${TRANSFER_PORT:-}" ] && echo "TRANSFER_PORT env var must be set -- aborting" && exit 1
      [ -z "''${SYNC_HOST:-}" ] && echo "SYNC_HOST env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_SYNC_PORT:-}" ] && echo "MARLOWE_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_HEADER_SYNC_PORT:-}" ] && echo "MARLOWE_HEADER_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_BULK_SYNC_PORT:-}" ] && echo "MARLOWE_BULK_SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${MARLOWE_QUERY_PORT:-}" ] && echo "MARLOWE_QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$TX_HOST" "$TX_PORT"
      ${wait-for-tcp}/bin/wait-for-tcp "$CONTRACT_HOST" "$LOAD_PORT"
      ${wait-for-tcp}/bin/wait-for-tcp "$SYNC_HOST" "$MARLOWE_QUERY_PORT"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-proxy}"

      ${marlowe-proxy}/bin/marlowe-proxy \
        --host "$HOST" \
        --port "$PORT" \
        --port-traced "$TRACED_PORT" \
        --marlowe-sync-host "$SYNC_HOST" \
        --marlowe-sync-port "$MARLOWE_SYNC_PORT" \
        --marlowe-header-port "$MARLOWE_HEADER_SYNC_PORT" \
        --marlowe-bulk-port "$MARLOWE_BULK_SYNC_PORT" \
        --marlowe-query-port "$MARLOWE_QUERY_PORT" \
        --marlowe-contract-host "$CONTRACT_HOST" \
        --marlowe-load-port "$LOAD_PORT" \
        --marlowe-transfer-port "$TRANSFER_PORT" \
        --contract-query-port "$CONTRACT_QUERY_PORT" \
        --tx-host "$TX_HOST" \
        --tx-command-port "$TX_PORT" \
        --http-port "$HTTP_PORT"
    '';
  };

  marlowe-runtime = mkOperableWithProbes {
    package = marlowe-runtime;
    runtimeInputs = [ z3 marlowe-plutus.packages.marlowe-minting-validator ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, TRACED_PORT: network binding
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS, DB_HOST,
      # STORE_DIR: location of the contract store directory
      # HTTP_PORT: port number for the HTTP healthcheck server

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service
      # MAX_STORE_SIZE: the maximum size of the contract store, in bytes
      # MIN_CONTRACT_AGE: the minimum age of contracts in the store before they can be garbage collected, in seconds.

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${TRACED_PORT:-}" ] && echo "TRACED_PORT env var must be set -- aborting" && exit 1
      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1
      [ -z "''${STORE_DIR:-}" ] && echo "STORE_DIR env var must be set -- aborting" && exit 1
      [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1
      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      mkdir -p /tmp /store
      HOME="$(mktemp -d)" # Ensure HOME is writable for sqitch config
      export TZ=Etc/UTC
      sqitch config --user user.name chainindexer
      sqitch config --user user.email example@example.com

      DATABASE_URI=${database-uri}

      cd ${chain-sync-sqitch-plan-dir}
      sqitch --quiet deploy --target "$DATABASE_URI"
      cd -

      cd ${runtime-sqitch-plan-dir}
      sqitch --quiet deploy --target "$DATABASE_URI"
      cd -

      NODE_CONFIG_DIR=$(dirname "$NODE_CONFIG")
      BYRON_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ByronGenesisFile "$NODE_CONFIG")"
      SHELLEY_GENESIS_CONFIG="$NODE_CONFIG_DIR/$(jq -r .ShelleyGenesisFile "$NODE_CONFIG")"
      BYRON_GENESIS_HASH=$(jq -r '.ByronGenesisHash' "$NODE_CONFIG")
      if [[ $(jq -r .networkId "$SHELLEY_GENESIS_CONFIG") != "Mainnet" ]]
      then
        CARDANO_TESTNET_MAGIC=$(jq -r '.networkMagic' "$SHELLEY_GENESIS_CONFIG");
        export CARDANO_TESTNET_MAGIC
      fi

      ${wait-for-socket}/bin/wait-for-socket "$CARDANO_NODE_SOCKET_PATH"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-runtime}"
      export MAX_STORE_SIZE="''${MAX_STORE_SIZE:-8589934592}" # 8 GB
      export MIN_CONTRACT_AGE="''${MIN_CONTRACT_AGE:-86400s}" # 1 day in seconds

      ${marlowe-runtime}/bin/marlowe-runtime \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI" \
        --shelley-genesis-config-file "$SHELLEY_GENESIS_CONFIG" \
        --genesis-config-file "$BYRON_GENESIS_CONFIG" \
        --genesis-config-file-hash "$BYRON_GENESIS_HASH" \
        --store-dir "$STORE_DIR" \
        --host "$HOST" \
        --port "$PORT" \
        --port-traced "$TRACED_PORT" \
        --http-port "$HTTP_PORT" \
        --minting-policy-cmd marlowe-minting-validator \
        --min-contract-age "$MIN_CONTRACT_AGE" \
        --max-store-size "$MAX_STORE_SIZE"

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

      #################
      # OPTIONAL VARS #
      #################
      # OTEL_EXPORTER_OTLP_ENDPOINT: The url of the open telemetry collector
      # OTEL_SERVICE_NAME: The name of the open telemetry service

      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${RUNTIME_HOST:-}" ] && echo "RUNTIME_HOST env var must be set -- aborting" && exit 1
      [ -z "''${RUNTIME_PORT:-}" ] && echo "RUNTIME_PORT env var must be set -- aborting" && exit 1

      ${wait-for-tcp}/bin/wait-for-tcp "$RUNTIME_HOST" "$RUNTIME_PORT"

      export OTEL_SERVICE_NAME="''${OTEL_SERVICE_NAME:-marlowe-web-server}"

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

  marlowe-pipe = mkOperable {
    package = marlowe-pipe;
    runtimeScript = mkMarloweAppScript ''
      ${marlowe-pipe}/bin/marlowe-pipe \
        --timeout-seconds "$TIMEOUT_SECONDS" \
        --build-seconds "$BUILD_SECONDS" \
        --confirm-seconds "$CONFIRM_SECONDS" \
        --retry-seconds "$RETRY_SECONDS" \
        --retry-limit "$RETRY_LIMIT"
    '';
  };

  marlowe-scaling = mkOperable {
    package = marlowe-scaling;
    runtimeScript = mkMarloweAppScript ''
      #################
      # REQUIRED VARS #
      #################
      # CONTRACTS_PER_PARTY: The number of contracts to run sequentially for each party.

      [ -z "''${CONTRACTS_PER_PARTY:-}" ] && echo "CONTRACTS_PER_PARTY env var must be set -- aborting" && exit 1

      #################
      # OPTIONAL VARS #
      #################
      # PARTIES: ADDRESS=KEYFILE per party separated by spaces

      read -a -r args <<< "''${PARTIES:-}"

      ${marlowe-scaling}/bin/marlowe-scaling \
        --timeout-seconds "$TIMEOUT_SECONDS" \
        --build-seconds "$BUILD_SECONDS" \
        --confirm-seconds "$CONFIRM_SECONDS" \
        --retry-seconds "$RETRY_SECONDS" \
        --retry-limit "$RETRY_LIMIT" \
        "$CONTRACTS_PER_PARTY" \
        "''${args[@]}"
    '';
  };

  marlowe-oracle = mkOperable {
    package = marlowe-oracle;
    runtimeScript = mkMarloweAppScript ''
      #################
      # REQUIRED VARS #
      #################
      # ADDRESS: The Bech32 address of the oracle.
      # KEYFILE: The extended payment signing key file for the oracle.

      [ -z "''${ADDRESS:-}" ] && echo "ADDRESS env var must be set -- aborting" && exit 1
      [ -z "''${KEYFILE:-}" ] && echo "KEYFILE env var must be set -- aborting" && exit 1

      #################
      # OPTIONAL VARS #
      #################
      # POLLING_FREQUENCY: The polling frequency in seconds when waiting on Marlowe Runtime.
      # REQUEUING_FREQUENCY: The requeuing frequency in seconds for reviewing the progress of contracts on Marlowe Runtime.

      ${marlowe-oracle}/bin/marlowe-oracle \
        --timeout-seconds "$TIMEOUT_SECONDS" \
        --build-seconds "$BUILD_SECONDS" \
        --confirm-seconds "$CONFIRM_SECONDS" \
        --retry-seconds "$RETRY_SECONDS" \
        --retry-limit "$RETRY_LIMIT" \
        --polling "''${POLLING_FREQUENCY:-5}" \
        --requeue "''${REQUEUING_FREQUENCY:-20}" \
        "$ADDRESS" \
        "$KEYFILE"
    '';
  };

  marlowe-finder = mkOperable {
    package = marlowe-finder;
    runtimeScript = mkMarloweAppScript ''
      #################
      # OPTIONAL VARS #
      #################
      # POLLING_FREQUENCY: The polling frequency in seconds when waiting on Marlowe Runtime.
      # REQUEUING_FREQUENCY: The requeuing frequency in seconds for reviewing the progress of contracts on Marlowe Runtime.

      ${marlowe-finder}/bin/marlowe-finder \
        --timeout-seconds "$TIMEOUT_SECONDS" \
        --build-seconds "$BUILD_SECONDS" \
        --confirm-seconds "$CONFIRM_SECONDS" \
        --retry-seconds "$RETRY_SECONDS" \
        --retry-limit "$RETRY_LIMIT" \
        --polling "''${POLLING_FREQUENCY:-5}"
    '';
  };
}
