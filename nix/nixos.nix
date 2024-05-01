{ self, marlowe-plutus, cardano-node, ... }: { lib, config, pkgs, ... }: let
  inherit (lib) mkOption types mkIf attrNames listToAttrs imap0 foldr attrValues mapAttrs' makeBinPath concatStringsSep optionalAttrs mkForce optionals versionAtLeast;

  allowedEnvs = [ "preprod" "preview" "mainnet" ];

  envIdxs = listToAttrs (imap0 (idx: env: { name = env; value = idx; }) allowedEnvs);

  runtimeOptions = { name, ... }: {
    options = {
      network = mkOption {
        type = types.enum allowedEnvs;
        description = "The Cardano network to connect to";
      };

      domain = mkOption {
        type = types.str;
        default = name;
        description = "The domain to host the runtime HTTP server on";
      };

      flake = mkOption {
        type = types.attrs;
        default = self;
        description = "A Nix Flake for the runtime application to deploy";
      };

      marlowe-plutus-flake = mkOption {
        type = types.attrs;
        default = marlowe-plutus;
        description = "A Nix Flake for the marlowe-plutus project to use for scripts.";
      };
    };
  };

  cfg = config.marlowe.runtimes;

  neededEnvs = attrNames (foldr (x: acc: acc // { ${x.network} = null; }) {} (attrValues cfg));

  mkNode = env: {
    autoStart = true;
    config = {
      imports = [ config.marlowe.cardano-node-flake.nixosModules.cardano-node ];

      system.stateVersion = "23.11";

      services.cardano-node = {
        enable = true;
        environment = env;
        hostAddr = "0.0.0.0";
        port = 3001 + (envIdxs.${env});
        socketPath = "/var/lib/cardano-node/node.socket";
      };
    };
  };
in {
  options = {
    marlowe.runtimes = mkOption {
      type = types.attrsOf (types.submodule runtimeOptions);
      default = { };
      description = "Marlowe Runtime instances to run";
    };

    marlowe.cardano-node-flake = mkOption {
      type = types.attrs;
      default = cardano-node;
      description = "The Nix flake to use for cardano-node";
    };
  };

  config = mkIf (cfg != { }) {
    http-services.proxied-services = listToAttrs (imap0 (idx: name: let runtime = cfg.${name}; in {
      name = "marlowe-runtime-${name}-web";
      value = {
        inherit (runtime) domain;
        systemdConfig = port: {
          description = "Marlowe Runtime Web (${name})";
          # allow unlimited restarts
          unitConfig.StartLimitIntervalSec = 0;
          serviceConfig = {
            # Restart on exit, with exponential fallback up to an hour
            # runtime-web needs to wait for runtime, which can take arbitrarily
            # long to be ready (see comment there).
            Restart = "always";
            RestartMaxDelaySec= "1hour";
            RestartSteps = 10;

            ExecSearchPath = makeBinPath [ runtime.flake.packages.${pkgs.system}.marlowe-web-server ];
            DynamicUser = true;
            ExecStart = concatStringsSep " " [
              "marlowe-web-server"
              "--http-port" (toString port)
              "--marlowe-runtime-host" "127.0.0.1"
              "--marlowe-runtime-port" (toString (3701 + 3 * idx))
              "--enable-open-api"
              "--access-control-allow-origin-all"
            ];
          };
          requires = [ "marlowe-runtime-${name}.service" ];
          after = [ "marlowe-runtime-${name}.service" ];
        };
      };
    }) (attrNames cfg));
    services.postgresql = {
      enable = true;
      ensureDatabases = map (inst: "runtime-${inst}") (attrNames cfg);
      # TODO: Set up Unix/pq users for the runtime so it doesn't need to run as root
      identMap = ''
        superuser_map root    postgres
        superuser_map root    postgres
        superuser_map /^(.*)$ \1
      '';
      authentication = ''
        local all all peer map=superuser_map
      '';
      settings = {
        shared_buffers = "2GB";
        huge_pages = "try";
        temp_buffers = "2GB";
        max_prepared_transactions = 256;
        max_wal_size = "4GB";
        max_locks_per_transaction = 256;
        work_mem = "32768";
        maintenance_work_mem = "262144";
        max_pred_locks_per_transaction = 1024;
      };
    };

    systemd.services = listToAttrs (imap0 (idx: name: let
      inherit (pkgs) writeShellApplication sqitchPg z3 postgresql;
      runtimeCfg = cfg.${name};
      env = runtimeCfg.network;
      envConfig = config.containers."cardano-node-${env}".config.services.cardano-node.environments.${env};
      db = "postgresql://postgres@/runtime-${name}";
      sqitch-conf = builtins.toFile "sqitch.conf" ''
        [user]
          name = chainindexer
          email = example@example.com
      '';
      runSqitch = writeShellApplication {
        name = "run-sqitch";
        text = ''
          export SQITCH_USER_CONFIG=${sqitch-conf}
          export TZ=Etc/UTC

          cd ${runtimeCfg.flake.sqitch-plan-dirs.chain-sync}
          sqitch --quiet deploy --target ${db}

          cd ${runtimeCfg.flake.sqitch-plan-dirs.runtime}
          sqitch --quiet deploy --target ${db}
        '';
        runtimeInputs = [ sqitchPg postgresql ];
      };
      pkg = runtimeCfg.flake.packages.${pkgs.system}.marlowe-runtime;
    in {
      name = "marlowe-runtime-${name}";
      value = {
        description = "Marlowe Runtime (${name})";
        # allow unlimited restarts
        unitConfig.StartLimitIntervalSec = 0;
        serviceConfig = {
          # Restart on exit, with exponential fallback up to an hour
          # The runtime needs to wait for postgres and the node to be up (which
          # should be relatively quick) /and/ for the node to be in Babbage (which
          # can take an arbitrarily long time).
          Restart = "always";
          RestartMaxDelaySec= "1hour";
          RestartSteps = 10;

          ExecSearchPath = makeBinPath [ runSqitch pkg ];
          # TODO: Wait for the nodes to be in babbage to start
          ExecStartPre = "run-sqitch";
          ExecStart = concatStringsSep " " ([
            "marlowe-runtime"
            "--socket-path" "/var/lib/nixos-containers/cardano-node-${env}/var/lib/cardano-node/node.socket"
            "--database-uri" db
            "--shelley-genesis-config-file" envConfig.networkConfig.ShelleyGenesisFile
            "--genesis-config-file" envConfig.networkConfig.ByronGenesisFile
            "--genesis-config-file-hash" envConfig.networkConfig.ByronGenesisHash
            "--store-dir" "/var/lib/marlowe-runtime-${name}/store"
            "--host" "0.0.0.0"
            "--port" (toString (3700 + 3 * idx))
            "--port-traced" (toString (3701 + 3 * idx))
            "--http-port" (toString (3702 + 3 * idx))
            "--minting-policy-cmd" "marlowe-minting-validator"
          ] ++ optionals (versionAtLeast pkg.version "0.6") [
            "--min-contract-age" "87400s"
            "--max-store-size" "8589934592"
          ]);
        };
        requires = [ "container@cardano-node-${env}.service" "postgresql.service" ];
        after = [ "container@cardano-node-${env}.service" "postgresql.service" ];
        environment = optionalAttrs (envConfig.networkConfig.RequiresNetworkMagic == "RequiresMagic") {
          CARDANO_TESTNET_MAGIC = toString (builtins.fromJSON (builtins.readFile envConfig.networkConfig.ShelleyGenesisFile)).networkMagic;
        };
        path = [ z3 runtimeCfg.marlowe-plutus-flake.packages.${pkgs.system}.marlowe-minting-validator ];
      };
    }) (attrNames cfg));

    containers = listToAttrs (map (env: {
      name = "cardano-node-${env}";
      value = mkNode env;
    }) neededEnvs);
  };
}
