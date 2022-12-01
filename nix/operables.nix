{ inputs }:
let
  inherit (inputs) self std nixpkgs bitte-cells;
  inherit (self) packages;
  inherit (nixpkgs.legacyPackages) sqitchPg;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;

  # Ensure this path only changes when sqitch.plan file is updated
  sqitch-plan = (builtins.path {
    path = self;
    name = "marlowe-chain-sync-sqitch-plan";
    filter = path: type:
      path == "${self}/marlowe-chain-sync" ||
      path == "${self}/marlowe-chain-sync/sqitch.plan";
  }) + "/marlowe-chain-sync/sqitch.plan";
in {
  chainseekd = std.lib.ops.mkOperable {
    package = packages.chainseekd;
    runtimeInputs = [ sqitchPg srvaddr ];
    runtimeScript = ''
      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        # Find DB_HOST when running on bitte cluster with patroni
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces: PSQL_ADDR0=domain:port; PSQL_HOST0=domain; PSQL_PORT0=port
        DB_HOST=$PSQL_ADDR0
      fi

      DATABASE_URI=postgresql://$DB_USER:$DB_PASS@$DB_HOST/$DB_NAME
      sqitch deploy --target "$DATABASE_URI" --plan-file ${sqitch-plan}
      ${packages.chainseekd}/bin/chainseekd \
        --host "$HOST" \
        --port-number "$PORT" \
        --query-port-number "$QUERY_PORT" \
        --job-port-number "$JOB_PORT" \
        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
        --database-uri  "$DATABASE_URI" \
        --genesis-config-file "$GENESIS_CONFIG" \
        --genesis-config-file-hash "$GENESIS_HASH"
    '';
  };
  marlowe-history = std.lib.ops.mkOperable {
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
  marlowe-discovery = std.lib.ops.mkOperable {
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
  marlowe-tx = std.lib.ops.mkOperable {
    package = packages.marlowe-tx;
    runtimeScript = ''
      ${packages.marlowe-tx}/bin/marlowe-tx \
        --host "$HOST" \
        --command-port "$PORT" \
        --chain-seek-port-number "$CHAINSEEKD_PORT" \
        --chain-seek-query-port-number "$CHAINSEEKD_QUERY_PORT" \
        --chain-seek-command-port-number "$CHAINSEEKD_COMMAND_PORT" \
        --chain-seek-host "$CHAINSEEKD_HOST" \
        --history-sync-port "$HISTORY_SYNC_PORT" \
        --history-host "$HISTORY_HOST"
      '';
  };
}
