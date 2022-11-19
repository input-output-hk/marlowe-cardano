{ inputs }:

let
  inherit (inputs) self data-merge dapps-world cardano-world;
  inherit (cardano-world) cardano;
  inherit (dapps-world) cloud;
  inherit (cloud.constants) baseDomain;

  # marlowe namespace is created and configured in dapps-world

  mkRuntimeJob = environment:
    let
      jobname = "runtime-${environment}";
    in
    data-merge.merge
      (
        self.nomadChart (
          {
            inherit jobname;
            environment = environment;
            namespace = "marlowe";
            nodeClass = "marlowe";
            domain = "marlowe-${jobname}.${baseDomain}";
            scaling = 3;
          }
        )
      )
      {
        job.${jobname}.group.marlowe-runtime.task = {
          node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env = {
              LEDGER_SLOT = cardano.environments.${environment}.usePeersFromLedgerAfterSlot;
              ENVIRONMENT = environment;
              DATA_DIR = "/persist/node-${environment}";
              LOCAL_ROOTS_SRV_DNS = "_marlowe-${jobname}-node._tcp.service.consul";
              PUBLIC_ROOTS_SRV_DNS = "_${environment}-node._tcp.service.consul";
            };
            resources = {
              memory = 10000;
            };
          };
          chainseekd = {
            env = {
              DB_NAME = "${environment}_chainseek";
              GENESIS_CONFIG = "/persist/node-${environment}/config/custom/byron-genesis.json";
              GENESIS_HASH = cardano.environments.${environment}.nodeConfig.ByronGenesisHash;
              DB_HOST = "_infra-database._master.service.us-east-1.consul";
            };
          };
        };
      };
in
{
  runtime-preprod = mkRuntimeJob "preprod";
  runtime-preview = mkRuntimeJob "preview";
  runtime-mainnet = mkRuntimeJob "mainnet";
}
