{ inputs }:

let
  inherit (inputs) self data-merge dapps-world cardano-world nixpkgs;

  inherit (cardano-world) cardano;
  inherit (dapps-world) cloud;
  inherit (cloud.constants) baseDomain;

  getNetworkMagic = env: (builtins.fromJSON
    (builtins.readFile
      cardano.environments.${env}.networkConfig.ShelleyGenesisFile
    )).networkMagic;

  # marlowe namespace is created and configured in dapps-world

  mkRuntimeJob = environment:
    let
      jobname = "marlowe-runtime-${environment}";
    in
    data-merge.merge
      (
        self.nomadChart (
          {
            inherit jobname;
            namespace = "marlowe";
            nodeClass = "marlowe";
            domain = "${jobname}.${baseDomain}";
            scaling = 1;
          }
        )
      )
      {
        job.${jobname}.group.marlowe-runtime.task = {
          node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env = {
              ENVIRONMENT = environment;
              DATA_DIR = "/persist/${jobname}";
              LOCAL_ROOTS_SRV_DNS = "_${jobname}-node._tcp.service.consul";
              PUBLIC_ROOTS_SRV_DNS = "_${environment}-node._tcp.service.consul";
            };
          };
          chain-indexer = {
            env = {
              DB_NAME = "${environment}_chainseek";
              BYRON_GENESIS_CONFIG = "/persist/${jobname}/config/${environment}/byron-genesis.json";
              BYRON_GENESIS_HASH = cardano.environments.${environment}.nodeConfig.ByronGenesisHash;
              SHELLEY_GENESIS_CONFIG = "/persist/${jobname}/config/${environment}/shelley-genesis.json";
              CARDANO_TESTNET_MAGIC = getNetworkMagic environment;
              MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
            };
            template = data-merge.append [
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
          };
          chainseekd = {
            env = {
              DB_NAME = "${environment}_chainseek";
              MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
              CARDANO_TESTNET_MAGIC = getNetworkMagic environment;
            };
            template = data-merge.append [
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
          };
        };
      };
in
{
  marlowe-runtime-preprod = mkRuntimeJob "preprod";
  marlowe-runtime-preview = mkRuntimeJob "preview";
  marlowe-runtime-mainnet = mkRuntimeJob "mainnet";
}
