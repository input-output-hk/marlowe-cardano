{ inputs }:
let
  inherit (inputs) self cardano-world;
  inherit (cardano-world) cardano;
  inherit (cardano) environments;
  inherit (self) oci-images;

  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";

  createDBTemplate = environment:
    [
      {
        change_mode = "restart";
        data = ''
          {{- with secret "kv/data/chainseek/${environment}" }}
          DB_USER={{ .Data.data.pgUser }}
          DB_PASS={{ .Data.data.pgPass }}
          {{ end -}}
        '';
        destination = "/secrets/db-secrets.env";
        env = true;
      }
    ];

  getNetworkMagic = environment: (builtins.fromJSON
    (builtins.readFile
      cardano.environments.${environment}.networkConfig.ShelleyGenesisFile
    )).networkMagic;

in
rec {
  chain-indexer = { environment }: {
    env = {
      CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
      BYRON_GENESIS_CONFIG = "/persist/marlowe-runtime-${environment}/config/${environment}/byron-genesis.json";
      BYRON_GENESIS_HASH = cardano.environments.${environment}.nodeConfig.ByronGenesisHash;
      SHELLEY_GENESIS_CONFIG = "/persist/marlowe-runtime-${environment}/config/${environment}/shelley-genesis.json";
      CARDANO_TESTNET_MAGIC = getNetworkMagic environment;
      DB_NAME = "${environment}_chainseek";
      MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
    };
    template = createDBTemplate environment;
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
      policies = ["marlowe-runtime"];
    };
  };
  chainseekd = { environment }: {
    env = {
      HOST = "127.0.0.1";
      PORT = 8090;
      QUERY_PORT = 8091;
      JOB_PORT = 8092;
      CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
      CARDANO_TESTNET_MAGIC = getNetworkMagic environment;
      DB_NAME = "${environment}_chainseek";
      MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
    };
    template = createDBTemplate environment;
    config.image = ociNamer oci-images.chainseekd;
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
  marlowe-history = {
    env = {
      HOST = "127.0.0.1";
      PORT = 9080;
      QUERY_PORT = 9081;
      SYNC_PORT = 9082;
      CHAINSEEKD_HOST = "127.0.0.1";
      CHAINSEEKD_PORT = "\${chainseekd.env.PORT}";
      CHAINSEEKD_QUERY_PORT = "\${chainseekd.env.QUERY_PORT}";
    };

    config.image = ociNamer oci-images.marlowe-history;
    config.ports = ["http"];
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
  marlowe-discovery = {
    env = {
      HOST = "127.0.0.1";
      PORT = 10081;
      QUERY_PORT = 10082;
      SYNC_PORT = 10083;
      CHAINSEEKD_HOST = "127.0.0.1";
      CHAINSEEKD_PORT = "\${chainseekd.env.PORT}";
      CHAINSEEKD_QUERY_PORT = "\${chainseekd.env.QUERY_PORT}";
    };

    config.image = ociNamer oci-images.marlowe-discovery;
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
  marlowe-tx = {
    env = {
      HOST = "127.0.0.1";
      PORT = 11080;
      CHAINSEEKD_HOST = "127.0.0.1";
      CHAINSEEKD_PORT = "\${chainseekd.env.PORT}";
      CHAINSEEKD_QUERY_PORT = "\${chainseekd.env.QUERY_PORT}";
      CHAINSEEKD_COMMAND_PORT = "\${chainseekd.env.JOB_PORT}";
      HISTORY_HOST = "127.0.0.1";
      HISTORY_SYNC_PORT = marlowe-history.env.SYNC_PORT;
    };

    config.image = ociNamer oci-images.marlowe-tx;
    user = "0:0";
    driver = "docker";
    kill_signal = "SIGINT";
    kill_timeout = "30s";
    resources.cpu = 2000;
    resources.memory = 4096;
  };
}
