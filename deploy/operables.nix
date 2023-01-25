{ inputs }:
let
  inherit (inputs) self std nixpkgs nixpkgs-unstable bitte-cells;
  inherit (self) packages;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.legacyPackages) jq sqitchPg postgresql coreutils;
  inherit (nixpkgs-unstable.legacyPackages) norouter;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;

  # Ensure this path only changes when sqitch.plan file is updated
  sqitch-plan-dir = (builtins.path {
    path = self;
    name = "marlowe-chain-sync-sqitch-plan";
    filter = path: type:
      path == "${self}/marlowe-chain-sync"
        || path == "${self}/marlowe-chain-sync/sqitch.plan"
        || lib.hasPrefix "${self}/marlowe-chain-sync/deploy" path
        || lib.hasPrefix "${self}/marlowe-chain-sync/revert" path;
  }) + "/marlowe-chain-sync";

  database-uri = "postgresql://$DB_USER:$DB_PASS@$DB_HOST/$DB_NAME";

  inherit (std.lib.ops) mkOperable;

in
{
  chain-indexer = mkOperable {
    package = packages.marlowe-chain-indexer;
    runtimeInputs = [ jq sqitchPg srvaddr postgresql coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # NODE_CONFIG: path to the cardano node config file
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS,
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1

      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      [ -z "''${DB_HOST:-}" ] && echo "DB_HOST env var must be set -- aborting" && exit 1

      DATABASE_URI=${database-uri}
      cd ${sqitch-plan-dir}
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

      ${packages.marlowe-chain-indexer}/bin/marlowe-chain-indexer \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI" \
        --shelley-genesis-config-file "$SHELLEY_GENESIS_CONFIG" \
        --genesis-config-file "$BYRON_GENESIS_CONFIG" \
        --genesis-config-file-hash "$BYRON_GENESIS_HASH"
    '';
  };

  chainseekd = mkOperable {
    package = packages.chainseekd;
    runtimeInputs = [ srvaddr jq coreutils ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, QUERY_PORT, JOB_PORT: network binding
      # CARDANO_NODE_SOCKET_PATH: path to the node socket
      # DB_NAME, DB_USER, DB_PASS,
      # Either DB_HOST or MASTER_REPLICA_SRV_DNS (for auto-discovery of DB host with srvaddr)

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${QUERY_PORT:-}" ] && echo "QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${JOB_PORT:-}" ] && echo "JOB_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ] && echo "CARDANO_NODE_SOCKET_PATH env var must be set -- aborting" && exit 1
      [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DB_USER:-}" ] && echo "DB_USER env var must be set -- aborting" && exit 1
      [ -z "''${DB_PASS:-}" ] && echo "DB_PASS env var must be set -- aborting" && exit 1

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

      DATABASE_URI=${database-uri}
      ${packages.chainseekd}/bin/chainseekd \
        --host "$HOST" \
        --port-number "$PORT" \
        --query-port-number "$QUERY_PORT" \
        --job-port-number "$JOB_PORT" \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI"
    '';
  };
  marlowe-history = mkOperable {
    package = packages.marlowe-history;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, QUERY_PORT, SYNC_PORT: network binding
      # CHAINSEEKD_HOST, CHAINSEEKD_PORT, CHAINSEEKD_QUERY_PORT: connection info to chainseekd

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${QUERY_PORT:-}" ] && echo "QUERY_PORT env var must be set -- aborting" && exit 1
      [ -z "''${SYNC_PORT:-}" ] && echo "SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_HOST:-}" ] && echo "CHAINSEEKD_HOST env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_PORT:-}" ] && echo "CHAINSEEKD_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_QUERY_PORT:-}" ] && echo "CHAINSEEKD_QUERY_PORT env var must be set -- aborting" && exit 1

      ${packages.marlowe-history}/bin/marlowe-history \
        --host "$HOST" \
        --command-port "$PORT" \
        --query-port "$QUERY_PORT" \
        --sync-port "$SYNC_PORT" \
        --chain-seek-port-number "$CHAINSEEKD_PORT" \
        --chain-seek-query-port-number "$CHAINSEEKD_QUERY_PORT" \
        --chain-seek-host "$CHAINSEEKD_HOST"
    '';
  };
  marlowe-discovery = mkOperable {
    package = packages.marlowe-discovery;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT, SYNC_PORT: network binding
      # CHAINSEEKD_HOST, CHAINSEEKD_PORT, CHAINSEEKD_QUERY_PORT: connection info to chainseekd

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${SYNC_PORT:-}" ] && echo "SYNC_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_HOST:-}" ] && echo "CHAINSEEKD_HOST env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_PORT:-}" ] && echo "CHAINSEEKD_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_QUERY_PORT:-}" ] && echo "CHAINSEEKD_QUERY_PORT env var must be set -- aborting" && exit 1

      ${packages.marlowe-discovery}/bin/marlowe-discovery \
        --host "$HOST" \
        --query-port "$PORT" \
        --sync-port "$SYNC_PORT" \
        --chain-seek-port-number "$CHAINSEEKD_PORT" \
        --chain-seek-query-port-number "$CHAINSEEKD_QUERY_PORT" \
        --chain-seek-host "$CHAINSEEKD_HOST"
    '';
  };
  marlowe-tx = mkOperable {
    package = packages.marlowe-tx;
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # HOST, PORT: network binding
      # CHAINSEEKD_HOST, CHAINSEEKD_PORT, CHAINSEEKD_QUERY_PORT, CHAINSEEKD_COMMAND_PORT: connection info to chainseekd

      [ -z "''${HOST:-}" ] && echo "HOST env var must be set -- aborting" && exit 1
      [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_HOST:-}" ] && echo "CHAINSEEKD_HOST env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_PORT:-}" ] && echo "CHAINSEEKD_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_COMMAND_PORT:-}" ] && echo "CHAINSEEKD_COMMAND_PORT env var must be set -- aborting" && exit 1
      [ -z "''${CHAINSEEKD_QUERY_PORT:-}" ] && echo "CHAINSEEKD_QUERY_PORT env var must be set -- aborting" && exit 1

      ${packages.marlowe-tx}/bin/marlowe-tx \
        --host "$HOST" \
        --command-port "$PORT" \
        --chain-seek-port-number "$CHAINSEEKD_PORT" \
        --chain-seek-query-port-number "$CHAINSEEKD_QUERY_PORT" \
        --chain-seek-command-port-number "$CHAINSEEKD_COMMAND_PORT" \
        --chain-seek-host "$CHAINSEEKD_HOST" \
    '';
  };
}
