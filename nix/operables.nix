{ inputs }:
let
  inherit (inputs) std nixpkgs self;
  inherit (self) packages;
  inherit (nixpkgs.legacyPackages) sqitchPg;
  dbConnectionScript = ''
    [ -z "''${MASTER_REPLICA_SRV_DNS:-}" ] && echo "MASTER_REPLICA_SRV_DNS env var must be set -- aborting" && exit 1
    [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
    eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
    # produces:
    # PSQL_ADDR0=domain:port
    # PSQL_HOST0=domain
    # PSQL_PORT0=port
    DATABASE_URI=postgresql://$DB_USER:$DB_PASS@$PSQL_ADDR0/$DB_NAME
  '';
in {
  chainseekd = std.lib.ops.mkOperable {
    package = packages.chainseekd;
    runtimeInputs = [ sqitchPg ];
    runtimeScript = ''
      ${dbConnectionScript}
      sqitch --target $DATABASE_URI --plan-file ${self}/marlowe-chain-sync/sqitch.plan
      ${packages.chainseekd}/bin/chainseekd \
        --host $HOST \
        --port-number $PORT \
        --query-port-number $QUERY_PORT \
        --job-port-number $JOB_PORT \
        --socket-path $CARDANO_NODE_SOCKET_PATH \
        --database-uri  $DATABASE_URI \
        --genesis-config-file $GENESIS_CONFIG \
        --genesis-config-file-hash $GENESIS_HASH
    '';
  };
  marlowe-history = std.lib.ops.mkOperable {
    package = packages.marlowe-history;
    runtimeScript = ''
      ${packages.marlowe-history}/bin/marlowe-history \
        --host $HOST \
        --command-port $PORT \
        --query-port $QUERY_PORT \
        --sync-port $SYNC_PORT \
        --chain-seek-port-number $CHAINSEEKD_PORT \
        --chain-seek-query-port-number $CHAINSEEKD_QUERY_PORT \
        --chain-seek-host $CHAINSEEKD_HOST
      '';
  };
  marlowe-discovery = std.lib.ops.mkOperable {
    package = packages.marlowe-discovery;
    runtimeScript = ''
      ${packages.marlowe-discovery}/bin/marlowe-discovery \
        --host $HOST \
        --query-port $PORT \
        --sync-port $SYNC_PORT \
        --chain-seek-port-number $CHAINSEEKD_PORT \
        --chain-seek-query-port-number $CHAINSEEKD_QUERY_PORT \
        --chain-seek-host $CHAINSEEKD_HOST
      '';
  };
  marlowe-tx = std.lib.ops.mkOperable {
    package = packages.marlowe-tx;
    runtimeScript = ''
      ${packages.marlowe-tx}/bin/marlowe-tx \
        --host $HOST \
        --command-port $PORT \
        --chain-seek-port-number $CHAINSEEKD_PORT \
        --chain-seek-query-port-number $CHAINSEEKD_QUERY_PORT \
        --chain-seek-command-port-number $CHAINSEEKD_COMMAND_PORT \
        --chain-seek-host $CHAINSEEKD_HOST \
        --history-sync-port $HISTORY_SYNC_PORT \
        --history-host $HISTORY_HOST
      '';
  };
}
