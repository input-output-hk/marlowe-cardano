{ sqitchPg, runCommand, writeShellScriptBin, writeText, lib, glibcLocales, nix, git, networks, su-exec }:
let
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
  '';

  node-service = network: {
    image = "inputoutput/cardano-node:1.35.3-configs";

    environment = [
      "NETWORK=${network.name}"
    ];

    volumes = [
      # TODO should be possible to do this with one shared volume, alas the image doesn't let you override the socket path
      "shared-${network.name}:/ipc"
      "node-${network.name}-db:/opt/cardano/data"
    ];
  };

  chainseekd-service = network: {
    image = "alpine:3.16.2";

    volumes = [
      "./:/src"
      "/nix:/nix"
      "\${HOME}/.cabal:\${HOME}/.cabal"
      "${symlinks}:/exec"
      "shared-${network.name}:/ipc"
      "/etc/passwd:/etc/passwd:ro"
      "/etc/group:/etc/group:ro"
    ];

    environment = [
      "CABAL_DIR=\${HOME}/.cabal"
      "REAL_USER=\${USER}"
      "REAL_HOME=\${HOME}"
    ];

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
    ];

    ports = map toString [
      3715
      3716
      3720
    ];

    restart = "unless-stopped";
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

    # TODO: Multiple networks on same pq or multiple pqs
    # TODO: ensure sqitch
    services.chainseekd-preview = chainseekd-service networks.preview;

    services.node-preprod = node-service networks.preprod;
    volumes.shared-preprod = null;
    volumes.node-preprod-db = null;

    services.node-preview = node-service networks.preview;
    volumes.shared-preview = null;
    volumes.node-preview-db = null;
  };
in
writeText "compose.yaml" (builtins.toJSON spec)
