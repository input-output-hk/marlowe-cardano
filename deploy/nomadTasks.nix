{ inputs }:
let
  inherit (inputs) self cardano-world;
  inherit (self) oci-images;

  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";

  dbTemplate =
    [
      {
        change_mode = "restart";
        data = ''
          {{- with secret (printf "kv/data/chainsync/%s" (env "NOMAD_META_environment")) }}
          DB_USER={{ .Data.data.pgUser }}
          DB_PASS={{ .Data.data.pgPass }}
          {{ end -}}
        '';
        destination = "/secrets/db-secrets.env";
        env = true;
      }
    ];

  nodeConfigDir = "/persist/marlowe-runtime-\${NOMAD_META_environment}/config/\${NOMAD_META_environment}/";
in
rec {
  chain-indexer = {
    env =
      let
        nodeConfigDir = "/persist/marlowe-runtime-\${NOMAD_META_environment}/config/\${NOMAD_META_environment}/";
      in
      {
        CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
        NODE_CONFIG = "${nodeConfigDir}/config.json";
        DB_NAME = "\${NOMAD_META_environment}_chainsync";
        MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
      };
    template = dbTemplate;
    config.image = ociNamer oci-images.chain-indexer;
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
      policies = [ "marlowe-runtime" ];
    };
  };
  marlowe-chain-sync = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
      JOB_PORT = "\${NOMAD_PORT_marlowe_chain_sync_command}";

      CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
      NODE_CONFIG = "${nodeConfigDir}/config.json"; # To get network magic

      DB_NAME = "\${NOMAD_META_environment}_chainsync";
      MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
    };
    template = dbTemplate;
    config.image = ociNamer oci-images.marlowe-chain-sync;
    config.ports = [ "marlowe-chain-sync" "marlowe_chain_sync_query" "marlowe_chain_sync_command" ];
    service.port = "marlowe-chain-sync";
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
      policies = [ "marlowe-runtime" ];
    };
  };
  marlowe-history = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_history}";
      QUERY_PORT = "\${NOMAD_PORT_history_query}";
      SYNC_PORT = "\${NOMAD_PORT_history_sync}";
      MARLOWE_CHAIN_SYNC_HOST = "localhost";
      MARLOWE_CHAIN_SYNC_PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      MARLOWE_CHAIN_SYNC_QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
    };

    config.image = ociNamer oci-images.marlowe-history;
    config.ports = [ "history" "history_query" "history_sync" ];
    service.port = "history";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
  marlowe-discovery = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_discovery}";
      QUERY_PORT = "\${NOMAD_PORT_discovery_query}";
      SYNC_PORT = "\${NOMAD_PORT_discovery_sync}";
      MARLOWE_CHAIN_SYNC_HOST = "localhost";
      MARLOWE_CHAIN_SYNC_PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      MARLOWE_CHAIN_SYNC_QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
    };

    config.image = ociNamer oci-images.marlowe-discovery;
    config.ports = [ "discovery" "discovery_query" "discovery_sync" ];
    service.port = "discovery";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
  marlowe-tx = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_tx}";
      MARLOWE_CHAIN_SYNC_HOST = "localhost";
      MARLOWE_CHAIN_SYNC_PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      MARLOWE_CHAIN_SYNC_QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
      MARLOWE_CHAIN_SYNC_COMMAND_PORT = "\${NOMAD_PORT_marlowe_chain_sync_command}";
      HISTORY_HOST = "\${NOMAD_IP_history}";
      HISTORY_SYNC_PORT = "\${NOMAD_PORT_history_sync}";
    };

    config.image = ociNamer oci-images.marlowe-tx;
    config.ports = [ "tx" ];
    service.port = "tx";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
}
