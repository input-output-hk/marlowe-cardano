{ inputs }:
let
  inherit (inputs) self cardano-world;
  inherit (self) oci-images;

  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "ghcr.io/input-output-hk/${oci.imageName}:latest";

  dbTemplate = db:
    [
      {
        change_mode = "restart";
        data = ''
          {{- with secret (printf "kv/data/${db}/%s" (env "NOMAD_META_environment")) }}
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
rec
{
  marlowe-chain-indexer = {
    env =
      let
        nodeConfigDir = "/persist/marlowe-runtime-\${NOMAD_META_environment}/config/\${NOMAD_META_environment}/";
      in
      {
        CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
        NODE_CONFIG = "${nodeConfigDir}/config.json";
        DB_NAME = "\${NOMAD_META_environment}_chainsync";
        MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
        HTTP_PORT = "\${NOMAD_PORT_chain_indexer_http}";
      };
    template = dbTemplate "chainsync";
    config.image = ociNamer oci-images.marlowe-chain-indexer;
    config.ports = [ "chain_indexer_http" ];
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 2048;
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
      HTTP_PORT = "\${NOMAD_PORT_chain_sync_http}";
    };
    template = dbTemplate "chainsync";
    config.image = ociNamer oci-images.marlowe-chain-sync;
    config.ports = [ "marlowe_chain_sync" "marlowe_chain_sync_query" "marlowe_chain_sync_command" "chain_sync_http" ];
    service.port = "marlowe_chain_sync";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 2048;
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

  marlowe-indexer = {
    env = {
      DB_NAME = "\${NOMAD_META_environment}_marlowe";
      MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
      MARLOWE_CHAIN_SYNC_HOST = "localhost";
      MARLOWE_CHAIN_SYNC_PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      MARLOWE_CHAIN_SYNC_QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
      HTTP_PORT = "\${NOMAD_PORT_indexer_http}";
    };
    template = dbTemplate "marlowe";
    config.image = ociNamer oci-images.marlowe-indexer;
    config.ports = [ "indexer_http" ];
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 1000;
    resources.memory = 1024;
    vault = {
      change_mode = "noop";
      env = true;
      policies = [ "marlowe-runtime" ];
    };
  };

  marlowe-sync = {
    env = {
      DB_NAME = "\${NOMAD_META_environment}_marlowe";
      MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
      HOST = "0.0.0.0";
      MARLOWE_SYNC_PORT = "\${NOMAD_PORT_marlowe_sync}";
      MARLOWE_HEADER_SYNC_PORT = "\${NOMAD_PORT_marlowe_header_sync}";
      MARLOWE_QUERY_PORT = "\${NOMAD_PORT_marlowe_query}";
      HTTP_PORT = "\${NOMAD_PORT_sync_http}";
    };
    template = dbTemplate "marlowe";
    config.image = ociNamer oci-images.marlowe-sync;
    config.ports = [ "marlowe_sync" "marlowe_header_sync" "marlowe_query" "sync_http" ];
    service.port = "marlowe_sync";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 1000;
    resources.memory = 1024;
    vault = {
      change_mode = "noop";
      env = true;
      policies = [ "marlowe-runtime" ];
    };
  };

  marlowe-tx = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_tx}";
      MARLOWE_CHAIN_SYNC_HOST = "localhost";
      MARLOWE_CHAIN_SYNC_PORT = "\${NOMAD_PORT_marlowe_chain_sync}";
      MARLOWE_CHAIN_SYNC_QUERY_PORT = "\${NOMAD_PORT_marlowe_chain_sync_query}";
      MARLOWE_CHAIN_SYNC_COMMAND_PORT = "\${NOMAD_PORT_marlowe_chain_sync_command}";
      HTTP_PORT = "\${NOMAD_PORT_tx_http}";
    };
    config.image = ociNamer oci-images.marlowe-tx;
    config.ports = [ "tx" "tx_http" ];
    service.port = "tx";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 1000;
    resources.memory = 1024;
  };

  marlowe-proxy = {
    env = {
      HOST = "0.0.0.0";
      PORT = "\${NOMAD_PORT_proxy}";
      TX_HOST = "localhost";
      TX_PORT = "\${NOMAD_PORT_tx}";
      SYNC_HOST = "localhost";
      MARLOWE_SYNC_PORT = "\${NOMAD_PORT_marlowe_sync}";
      MARLOWE_HEADER_SYNC_PORT = "\${NOMAD_PORT_marlowe_header_sync}";
      MARLOWE_QUERY_PORT = "\${NOMAD_PORT_marlowe_query}";
      HTTP_PORT = "\${NOMAD_PORT_proxy_http}";
    };
    config.image = ociNamer oci-images.marlowe-proxy;
    config.ports = [ "proxy" "proxy_http" ];
    service.port = "proxy";
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 1000;
    resources.memory = 1024;
  };
}
