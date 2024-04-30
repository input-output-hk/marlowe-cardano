{ self, marlowe-plutus, cardano-node, ... }: { lib, config, pkgs, ... }: let
  inherit (lib) mkOption types mkIf attrNames listToAttrs imap0 foldr attrValues mapAttrs' makeBinPath concatStringsSep optionalAttrs mkForce;

  allowedEnvs = [ "preprod" "preview" "mainnet" ];

  envIdxs = listToAttrs (imap0 (idx: env: { name = env; value = idx; }) allowedEnvs);

  runtimeOptions.options = {
    network = mkOption {
      type = types.enum allowedEnvs;
      description = "The Cardano network to connect to";
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
    in {
      name = "marlowe-runtime-${name}";
      value = {
        description = "Marlowe Runtime (${name})";
        serviceConfig = {
          ExecSearchPath = makeBinPath [ runSqitch runtimeCfg.flake.packages.${pkgs.system}.marlowe-runtime ];
          # TODO: Wait for the nodes to be in babbage to start
          ExecStartPre = "run-sqitch";
          ExecStart = concatStringsSep " " [
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
            "--min-contract-age" "87400s"
            "--max-store-size" "8589934592"
          ];
        };
        wantedBy = [ "multi-user.target" ];
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
