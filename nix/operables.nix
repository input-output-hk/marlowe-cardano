{ inputs }:
let
  inherit (inputs) self std nixpkgs bitte-cells;
  inherit (self) packages;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.legacyPackages) sqitchPg postgresql norouter;
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

  mkOperable = args: std.lib.ops.mkOperable (args // {
    debugInputs = [ norouter ] ++ (args.debugInputs or []);
  });

in {
  chain-indexer = mkOperable {
    package = packages.marlowe-chain-indexer;
    runtimeInputs = [ sqitchPg srvaddr postgresql ];
    runtimeScript = ''
      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      DATABASE_URI=${database-uri}
      cd ${sqitch-plan-dir}
      export TZ=Etc/UTC
      sqitch config --user user.name chainindexer
      sqitch config --user user.email example@example.com
      sqitch --quiet deploy --target "$DATABASE_URI"
      cd -

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
    runtimeInputs = [ srvaddr ];
    runtimeScript = ''
      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

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
