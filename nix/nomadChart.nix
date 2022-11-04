{ inputs }:
let
  inherit (inputs) data-merge cells nixpkgs self cardano-world;
  inherit (inputs.bitte-cells) vector _utils;
  inherit (self) oci-images;
  l = nixpkgs.lib // builtins;
  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
in
  {
    jobname ? "marlowe-runtime",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-1"],
    domain,
    extraVector ? {},
    nodeClass,
    scaling,
  } @ args: let
    id = jobname;
    type = "service";
    priority = 50;
    vaultPkiPath = "pki/issue/marlowe-runtime";
    consulRolePath = "consul/creds/marlowe-runtime";
  in
    with data-merge; {
      job.${id} = {
        inherit namespace datacenters id type priority;
        # ----------
        # Scheduling
        # ----------
        constraint = [
          {
            attribute = "\${node.class}";
            operator = "=";
            value = "${nodeClass}";
          }
          # {
          #   attribute = "\${meta.cardano}";
          #   operator = "is_set";
          # }
          {
            operator = "distinct_hosts";
            value = "true";
          }
        ];
        spread = [{attribute = "\${node.datacenter}";}];
        # ----------
        # Update
        # ----------
        update.health_check = "task_states";
        update.healthy_deadline = "5m0s";
        update.max_parallel = 1;
        update.min_healthy_time = "10s";
        update.progress_deadline = "60m0s";
        update.stagger = "30s";
        # ----------
        # Migrate
        # ----------
        migrate.health_check = "checks";
        migrate.healthy_deadline = "8m20s";
        migrate.max_parallel = 1;
        migrate.min_healthy_time = "10s";
        # ----------
        # Reschedule
        # ----------
        reschedule.delay = "30s";
        reschedule.delay_function = "exponential";
        reschedule.max_delay = "1h0m0s";
        reschedule.unlimited = true;
        # ----------
        # Task Groups
        # ----------
        group.marlowe-runtime = let
          # work-around: we need to get rid of vector first
          dbsyncFullGroup = (cardano-world.cardano.nomadCharts.cardano-db-sync (args // {jobname = "db-sync";})).job.db-sync.group.db-sync;
          dbsyncWithoutTask = l.removeAttrs dbsyncFullGroup ["task"];
          dbsync = dbsyncWithoutTask // {
            task.cardano-db-sync = dbsyncFullGroup.task.db-sync;
            task.node = dbsyncFullGroup.task.node;
          };
        in
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # prometheus metrics for cardano-graphql
              "http://127.0.0.1:8090/metrics"
            ];
            extra = extraVector;
          })
          (
            merge dbsync
              {
                count = scaling;
                # service = append [
                #   (import ./srv-cardano-graphql.nix {inherit namespace healthChecks;})
                # ];
                network.port.http.to = "3100";
                /* service = append [
                  {
                    name = "cardano-graphql-${namespace}";
                    port = "http";
                    tags = [
                      "ingress"
                      "traefik.enable=true"
                      "traefik.http.routers.cardano-graphql-${namespace}.rule=Host(`graphql.${domain}`) && PathPrefix(`/`)"
                      "traefik.http.routers.cardano-graphql-${namespace}.entrypoints=https"
                      "traefik.http.routers.cardano-graphql-${namespace}.tls.certresolver=acme"
                    ];
                    check = [
                      {
                        type = "tcp";
                        port = "http";
                        interval = "10s";
                        timeout = "2s";
                      }
                    ];
                  }
                ];*/

                task = rec {
                  # ----------
                  # Task: Marlowe-Runtime
                  # ----------
                  chainseekd = {
                    env = {
                      HOST = "127.0.0.1";
                      PORT = "8090";
                      QUERY_PORT = "8091";
                      JOB_PORT = "8092";
                      CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
                      GENESIS_CONFIG = "/persist/config/custom/byron-genesis.json";
                      GENESIS_HASH = cardano-world.cardano.environments.${namespace}.nodeConfig.ByronGenesisHash;
                      WORKLOAD_CACERT = "/secrets/tls/ca.pem";
                      WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
                      WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";
                    };

                    template =
                       _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
                      ++ _utils.nomadFragments.workload-identity-vault-consul {inherit consulRolePath;}
                      ++ [
                        {
                          change_mode = "restart";
                          data = "{{- with secret \"kv/data/marlowe-runtime/${namespace}\" }}{{ .Data.data.pgPass }}{{ end -}}";
                          env = "DB_USER";
                        }
                        {
                          change_mode = "restart";
                          data = "{{- with secret \"kv/data/marlowe-runtime/${namespace}\" }}{{ .Data.data.pgUser }}{{ end -}}";
                          env = "DB_PASS";
                        }
                      ];

                    config.image = ociNamer oci-images.chainseekd;
                    config.ports = ["http"];
                    user = "0:0";
                    driver = "docker";
                    kill_signal = "SIGINT";
                    kill_timeout = "30s";
                    resources.cpu = 2000;
                    resources.memory = 4096;
                    volume_mount = {
                      destination = "/persist";
                      propagation_mode = "private";
                      volume = "persist-cardano-node-local";
                    };
                    vault = {
                      change_mode = "noop";
                      env = true;
                      policies = ["marlowe-runtime"];
                    };
                  };
                  marlowe-history = {
                    env = {
                      PORT = 9080;
                      QUERY_PORT = 9081;
                      SYNC_PORT = 9082;
                      CHAINSEEKD_HOST = "127.0.0.1";
                      CHAINSEEKD_PORT = chainseekd.env.PORT;
                      CHAINSEEKD_QUERY_PORT = chainseekd.env.QUERY_PORT;
                    };

                    config.image = ociNamer oci-images.marlowe-history;
                    config.ports = ["http"];
                    user = "0:0";
                    driver = "docker";
                    kill_signal = "SIGINT";
                    kill_timeout = "30s";
                    resources.cpu = 2000;
                    resources.memory = 4096;
                    vault = {
                      change_mode = "noop";
                      env = true;
                      policies = ["marlowe-runtime"];
                    };
                  };
                  marlowe-discovery = {
                    env = {
                      HOST = "127.0.0.1";
                      QUERY_PORT = 9081;
                      SYNC_PORT = 9082;
                      CHAINSEEKD_HOST = "127.0.0.1";
                      CHAINSEEKD_PORT = chainseekd.env.PORT;
                      CHAINSEEKD_QUERY_PORT = chainseekd.env.QUERY_PORT;
                    };

                    config.image = ociNamer oci-images.marlowe-discovery;
                    config.ports = ["http"];
                    user = "0:0";
                    driver = "docker";
                    kill_signal = "SIGINT";
                    kill_timeout = "30s";
                    resources.cpu = 2000;
                    resources.memory = 4096;
                    vault = {
                      change_mode = "noop";
                      env = true;
                      policies = ["marlowe-runtime"];
                    };
                  };
                  marlowe-tx = {
                    env = {
                      HOST = "127.0.0.1";
                      PORT = 10080;
                      CHAINSEEKD_HOST = "127.0.0.1";
                      CHAINSEEKD_PORT = chainseekd.env.PORT;
                      CHAINSEEKD_QUERY_PORT = chainseekd.env.QUERY_PORT;
                      CHAINSEEKD_COMMAND_PORT = chainseekd.env.JOB_PORT;
                    };

                    config.image = ociNamer oci-images.marlowe-discovery;
                    config.ports = ["http"];
                    user = "0:0";
                    driver = "docker";
                    kill_signal = "SIGINT";
                    kill_timeout = "30s";
                    resources.cpu = 2000;
                    resources.memory = 4096;
                    vault = {
                      change_mode = "noop";
                      env = true;
                      policies = ["marlowe-runtime"];
                    };
                  };
                };
              }
          );
      };
    }
