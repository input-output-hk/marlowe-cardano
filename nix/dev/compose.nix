{ sqitchPg, runCommand, writeShellScriptBin, writeText, lib, glibcLocales, nix, git, networks, su-exec }:
let
  network = networks.preprod;
  run-sqitch = writeShellScriptBin "run-sqitch" ''
    export PATH="$PATH:${lib.makeBinPath [ sqitchPg ]}"
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    cd /src/marlowe-chain-sync
    exec sqitch deploy
  '';

  # TODO Fix "configuration changed" when swapping from inside and outside container
  run-local-service = prog: writeShellScriptBin "run-${prog}" ''
    export PATH="${lib.makeBinPath [ nix git su-exec ]}:$PATH"
    export NIX_CONFIG="experimental-features = flakes nix-command"
    export NIX_REMOTE=daemon

    chown $REAL_USER $REAL_HOME

    cd /src
    su-exec $REAL_USER nix develop --command -- bash -c "cabal build ${lib.escapeShellArg prog} && cabal list-bin ${lib.escapeShellArg prog} >/tmp/bin"
    exec -a ${lib.escapeShellArg prog} "$(cat /tmp/bin)" "$@"
  '';

  symlinks = runCommand "symlinks" { } ''
    mkdir -p $out
    ln -sv ${run-sqitch}/bin/run-sqitch $out
    ln -sv ${run-local-service "chainseekd"}/bin/run-chainseekd $out
    ln -sv ${run-local-service "marlowe-history"}/bin/run-marlowe-history $out
    ln -sv ${run-local-service "marlowe-discovery"}/bin/run-marlowe-discovery $out
    ln -sv ${run-local-service "marlowe-tx"}/bin/run-marlowe-tx $out
    ln -sv ${run-local-service "marlowe-web-server"}/bin/run-marlowe-web-server $out
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
  };

  dev-service = {
    image = "alpine:3.16.2";
    volumes = [
      "./:/src"
      "/nix:/nix"
      "\${HOME}/.cabal:\${HOME}/.cabal"
      "${symlinks}:/exec"
      "shared:/ipc"
      "/etc/passwd:/etc/passwd:ro"
      "/etc/group:/etc/group:ro"
    ];
    environment = [
      "CABAL_DIR=\${HOME}/.cabal"
      "REAL_USER=\${USER}"
      "REAL_HOME=\${HOME}"
    ];
    restart = "unless-stopped";
  };

  chainseekd-service = dev-service // {
    command = [
      "/exec/run-chainseekd"
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
      "--host"
      "0.0.0.0"
    ];
    ports = map toString [
      3715
      3716
      3720
    ];
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep 3715";
      interval = "5s";
      timeout = "5s";
      retries = 30;
    };
    depends_on = {
      postgres = {
        condition = "service_healthy";
      };
    };
  };

  history-service = dev-service // {
    command = [
      "/exec/run-marlowe-history"
      "--chain-seek-host"
      "chainseekd"
      "--host"
      "0.0.0.0"
    ];
    ports = map toString [
      3717
      3718
      3719
    ];
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep 3717";
      interval = "5s";
      timeout = "5s";
      retries = 30;
    };
    depends_on = {
      chainseekd = {
        condition = "service_healthy";
      };
    };
  };

  discovery-service = dev-service // {
    command = [
      "/exec/run-marlowe-discovery"
      "--chain-seek-host"
      "chainseekd"
      "--host"
      "0.0.0.0"
    ];
    ports = map toString [
      3721
      3722
    ];
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep 3721";
      interval = "5s";
      timeout = "5s";
      retries = 30;
    };
    depends_on = {
      chainseekd = {
        condition = "service_healthy";
      };
      marlowe-history = {
        condition = "service_healthy";
      };
    };
  };

  tx-service = dev-service // {
    command = [
      "/exec/run-marlowe-tx"
      "--chain-seek-host"
      "chainseekd"
      "--history-host"
      "marlowe-history"
      "--host"
      "0.0.0.0"
    ];
    ports = map toString [
      3723
    ];
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep 3723";
      interval = "5s";
      timeout = "5s";
      retries = 30;
    };
    depends_on = {
      chainseekd = {
        condition = "service_healthy";
      };
      marlowe-history = {
        condition = "service_healthy";
      };
    };
  };

  web-service = dev-service // {
    command = [
      "/exec/run-marlowe-web-server"
      "--history-host"
      "marlowe-history"
      "--discovery-host"
      "marlowe-discovery"
      "--tx-host"
      "marlowe-tx"
      "--enable-open-api"
    ];
    ports = map toString [
      8080
    ];
    healthcheck = {
      test = "netstat -tulpn | grep LISTEN | grep 8080";
      interval = "5s";
      timeout = "5s";
      retries = 30;
    };
    depends_on = {
      marlowe-history = {
        condition = "service_healthy";
      };
      marlowe-discovery = {
        condition = "service_healthy";
      };
      marlowe-tx = {
        condition = "service_healthy";
      };
    };
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
    };

    volumes.postgres = null;

    # TODO: ensure sqitch
    services.chainseekd = chainseekd-service;
    services.marlowe-history = history-service;
    services.marlowe-discovery = discovery-service;
    services.marlowe-tx = tx-service;
    services.web = web-service;

    services.node = node-service;
    volumes.shared = null;
    volumes.node-db = null;
  };
in
writeText "compose.yaml" (builtins.toJSON spec)
