{ inputs, pkgs }:

let
  inherit (pkgs) sqitchPg postgresql runCommand writeShellScriptBin writeText lib glibcLocales;
  network = inputs.self.networks.preview;

  mkSqitchRunner = name: path: writeShellScriptBin name ''
    export PATH="$PATH:${lib.makeBinPath [ sqitchPg postgresql ]}"
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    cd ${path}
    exec sqitch deploy "$@"
  '';

  run-sqitch = mkSqitchRunner "run-sqitch" "/src/marlowe-chain-sync";
  run-sqitch-marlowe-indexer = mkSqitchRunner "run-sqitch-marlowe-indexer" "/src/marlowe-runtime/marlowe-indexer";

  run-chain-indexer = writeShellScriptBin "run-marlowe-chain-indexer" ''
    set -e
    PROG=${lib.escapeShellArg "marlowe-chain-indexer"}
    PKG=${lib.escapeShellArg "marlowe-chain-sync"}-${lib.escapeShellArg marloweRuntimeVersion}
    cd /src
    # Hard-coding linux because this won't work on Mac anyway.
    # TODO find a setup that works on MacOS
    BIN=./dist-newstyle/build/x86_64-linux/ghc-8.10.7/$PKG/x/$PROG/build/$PROG/$PROG
    export PATH="$PATH:${lib.makeBinPath [ sqitchPg postgresql ]}"
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    cd marlowe-chain-sync
    sqitch deploy -h postgres
    cd /src
    exec -a $PROG $BIN "$@"
  '';

  run-indexer = writeShellScriptBin "run-marlowe-indexer" ''
    set -e
    PROG=${lib.escapeShellArg "marlowe-indexer"}
    PKG=${lib.escapeShellArg "marlowe-runtime"}-${lib.escapeShellArg marloweRuntimeVersion}
    cd /src
    # Hard-coding linux because this won't work on Mac anyway.
    # TODO find a setup that works on MacOS
    BIN=./dist-newstyle/build/x86_64-linux/ghc-8.10.7/$PKG/x/$PROG/build/$PROG/$PROG
    export PATH="$PATH:${lib.makeBinPath [ sqitchPg postgresql ]}"
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    cd marlowe-runtime/marlowe-indexer
    sqitch deploy -h postgres
    cd /src
    exec -a $PROG $BIN "$@"
  '';

  run-local-service = project: version: prog: writeShellScriptBin "run-${prog}" ''
    set -e
    PROG=${lib.escapeShellArg prog}
    PKG=${lib.escapeShellArg project}-${lib.escapeShellArg version}
    cd /src
    # Hard-coding linux because this won't work on Mac anyway.
    # TODO find a setup that works on MacOS
    BIN=./dist-newstyle/build/x86_64-linux/ghc-8.10.7/$PKG/x/$PROG/build/$PROG/$PROG
    exec -a $PROG $BIN "$@"
  '';

  # We assume that all the components are versioned together.
  marloweRuntimeVersion = "0.0.2";
  symlinks = runCommand "symlinks" { } ''
    mkdir -p $out
    ln -sv ${run-sqitch}/bin/run-sqitch $out
    ln -sv ${run-sqitch-marlowe-indexer}/bin/run-sqitch-marlowe-indexer $out
    ln -sv ${run-chain-indexer}/bin/run-marlowe-chain-indexer $out
    ln -sv ${run-local-service "marlowe-chain-sync" marloweRuntimeVersion "marlowe-chain-sync"}/bin/run-marlowe-chain-sync $out
    ln -sv ${run-local-service "marlowe-runtime" marloweRuntimeVersion "marlowe-sync"}/bin/run-marlowe-sync $out
    ln -sv ${run-local-service "marlowe-runtime" marloweRuntimeVersion "marlowe-tx"}/bin/run-marlowe-tx $out
    ln -sv ${run-local-service "marlowe-runtime-web" marloweRuntimeVersion "marlowe-web-server"}/bin/run-marlowe-web-server $out
    ln -sv ${run-indexer}/bin/run-marlowe-indexer $out
    ln -sv ${run-local-service "marlowe-runtime" marloweRuntimeVersion "marlowe-proxy"}/bin/run-marlowe-proxy $out
    ln -sv ${run-local-service "marlowe-runtime" marloweRuntimeVersion "marlowe-contract"}/bin/run-marlowe-contract $out
  '';

  node-service = {
    image = "inputoutput/cardano-node:1.35.4";

    environment = [
      "NETWORK=${network.name}"
    ];

    volumes = [
      "shared:/ipc"
      "node-db:/opt/cardano/data"
    ];

    # This should be in the dockerfile
    healthcheck = {
      test = "socat -u OPEN:/dev/null UNIX-CONNECT:/ipc/node.socket";
      interval = "10s";
      timeout = "5s";
      retries = 10;
    };
  };

  dev-service = { ports, depends_on ? [ ], command, environment ? [ ], volumes ? [ ] }: {
    inherit command;
    image = "alpine:3.16.2";
    volumes = [
      "./:/src"
      "/nix:/nix"
      "${symlinks}:/exec"
      "shared:/ipc"
    ] ++ volumes;
    restart = "unless-stopped";
    ports = map toString ports;
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep ${toString (builtins.head ports)}";
      interval = "10s";
      timeout = "5s";
      retries = 5;
    };
    depends_on = lib.genAttrs depends_on (_: { condition = "service_healthy"; });
    environment = [
      "OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318"
    ] ++ environment;
  };

  chain-indexer-service = {
    image = "alpine:3.16.2";
    volumes = [
      "./:/src"
      "/nix:/nix"
      "${symlinks}:/exec"
      "shared:/ipc"
    ];
    environment = [
      "TZ=UTC"
      "OTEL_SERVICE_NAME=marlowe-chain-indexer"
      "OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318"
    ];
    restart = "unless-stopped";
    depends_on = {
      "postgres" = { condition = "service_healthy"; };
      "node" = { condition = "service_healthy"; };
    };
    command = [
      "/exec/run-marlowe-chain-indexer"
      "--testnet-magic"
      (builtins.toString network.magic)
      "--socket-path"
      "/ipc/node.socket"
      "--database-uri"
      "postgresql://postgres@postgres/chain"
      "--genesis-config-file"
      network.nodeConfig.ByronGenesisFile
      "--genesis-config-file-hash"
      network.nodeConfig.ByronGenesisHash
      "--shelley-genesis-config-file"
      network.nodeConfig.ShelleyGenesisFile
    ];
  };

  marlowe-indexer-service = {
    image = "alpine:3.16.2";
    volumes = [
      "./:/src"
      "/nix:/nix"
      "${symlinks}:/exec"
      "shared:/ipc"
    ];
    environment = [
      "TZ=UTC"
      "OTEL_SERVICE_NAME=marlowe-indexer"
      "OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318"
    ];
    restart = "unless-stopped";
    depends_on = {
      "postgres" = { condition = "service_healthy"; };
      "marlowe-chain-sync" = { condition = "service_healthy"; };
    };
    command = [
      "/exec/run-marlowe-indexer"
      "--chain-sync-host"
      "marlowe-chain-sync"
      "--database-uri"
      "postgresql://postgres@postgres/chain"
    ];
  };

  marlowe-chain-sync-service = dev-service {
    ports = [ 3715 3716 3720 ];
    depends_on = [ "postgres" "node" ];
    command = [
      "/exec/run-marlowe-chain-sync"
      "--testnet-magic"
      (builtins.toString network.magic)
      "--socket-path"
      "/ipc/node.socket"
      "--database-uri"
      "postgresql://postgres@postgres/chain"
      "--host"
      "0.0.0.0"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-chain-sync" ];
  };

  sync-service = dev-service {
    ports = [ 3724 3725 3726 ];
    depends_on = [ "postgres" ];
    command = [
      "/exec/run-marlowe-sync"
      "--database-uri"
      "postgresql://postgres@postgres/chain"
      "--host"
      "0.0.0.0"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-sync" ];
  };

  contract-service = dev-service {
    ports = [ 3727 3728 ];
    command = [
      "/exec/run-marlowe-contract"
      "--store-dir"
      "/store"
      "--host"
      "0.0.0.0"
    ];
    volumes = [
      "marlowe-contract-store:/store"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-contract" ];
  };

  tx-service = dev-service {
    ports = [ 3723 ];
    depends_on = [ "marlowe-chain-sync" ];
    command = [
      "/exec/run-marlowe-tx"
      "--chain-sync-host"
      "marlowe-chain-sync"
      "--contract-host"
      "marlowe-contract"
      "--host"
      "0.0.0.0"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-tx" ];
  };

  proxy-service = dev-service {
    ports = [ 3700 3701 ];
    depends_on = [ "marlowe-sync" "marlowe-tx" "marlowe-contract" ];
    command = [
      "/exec/run-marlowe-proxy"
      "--host"
      "0.0.0.0"
      "--marlowe-sync-host"
      "marlowe-sync"
      "--tx-host"
      "marlowe-tx"
      "--marlowe-contract-host"
      "marlowe-contract"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-proxy" ];
  };

  web-service = dev-service {
    ports = [ 8080 ];
    depends_on = [ "marlowe-proxy" ];
    command = [
      "/exec/run-marlowe-web-server"
      "--marlowe-runtime-host"
      "marlowe-proxy"
      "--enable-open-api"
      "--access-control-allow-origin-all"
    ];
    environment = [ "OTEL_SERVICE_NAME=marlowe-web-server" ];
  };

  spec = {
    services.postgres = {
      image = "postgres:11.5-alpine";

      # TODO translate from attrset
      environment = [
        "POSTGRES_LOGGING=true"
        "POSTGRES_USER=postgres"
        "POSTGRES_HOST_AUTH_METHOD=trust"
        "TZ=UTC"
      ];

      # TODO strucutred volume definition
      # TODO Connect volumes here to top-level volumes
      volumes = [
        "postgres:/var/lib/postgresql/data"
        "./postgres/init.sql:/docker-entrypoint-initdb.d/init.sql"
        "./:/src"
        "/nix:/nix"
        "${symlinks}:/exec"
      ];

      # TODO enum
      restart = "unless-stopped";

      # this should be in the dockerfile...
      healthcheck = {
        test = [ "CMD" "pg_isready" "-U" "postgres" ];
        # TODO structured
        interval = "10s";
        timeout = "5s";
        retries = 5;
      };

      logging = {
        # enum
        driver = "json-file";
        options = {
          max-size = "200k";
          max-file = "10";
        };
      };

      command = [
        "-c"
        "max_connections=1000"
      ];
    };

    services.jaeger = {
      image = "jaegertracing/all-in-one";
      restart = "unless-stopped";
      command = [
        "--memory.max-traces"
        "10000"
        "--query.base-path"
        "/jaeger/ui"
      ];
      environment = [ "COLLECTOR_OTLP_ENABLED=true" ];
      ports = [ 16686 ];
    };

    services.otel-collector = {
      image = "otel/opentelemetry-collector-contrib:0.76.1";
      restart = "unless-stopped";
      command = [ "--config=/etc/otel-collector-config.yml" ];
      volumes = [ "./otel/otel-collector-config.yml:/etc/otel-collector-config.yml" ];
      depends_on = [ "jaeger" ];
    };

    volumes.postgres = null;

    services.marlowe-chain-indexer = chain-indexer-service;
    services.marlowe-chain-sync = marlowe-chain-sync-service;
    services.marlowe-tx = tx-service;
    services.marlowe-proxy = proxy-service;
    services.web = web-service;
    services.marlowe-indexer = marlowe-indexer-service;
    services.marlowe-sync = sync-service;
    services.marlowe-contract = contract-service;

    services.node = node-service;
    volumes.shared = null;
    volumes.node-db = null;
    volumes.marlowe-contract-store = null;
  };
in
writeText "compose.yaml" (builtins.toJSON spec)
