{ inputs }:
let
  inherit (inputs) self data-merge cardano-world;
  jobname = "marlowe-runtime";
in
{
  preprod = data-merge.merge (
    self.nomadChart (
        {
          inherit jobname;
          namespace = "preprod";
          nodeClass = "preprod";
          domain = "preprod.dapps.aws.iohkdev.io";
          scaling = 3;
        }
      )
    ) {
      job.${jobname}.group.marlowe-runtime.task = {
          node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env = {
              LEDGER_SLOT = cardano-world.environments.preprod.usePeersFromLedgerAfterSlot;
              ENVIRONMENT = "preprod";
              DATA_DIR = "/persist/node";
              # CONSUL_KV_PATH = "config/cardano/preprod";
              # VAULT_KV_PATH = "kv/data/cardano/preprod/sp-1";
              LOCAL_ROOTS_SRV_DNS = "_preprod-${jobname}-node._tcp.service.consul";
              PUBLIC_ROOTS_SRV_DNS = "_preprod-node._tcp.service.consul";
            };
            resources = {
              memory = 10240;
            };
          };
          chainseekd = {
            env = {
              DB_NAME = "preprod_chainseekd";
              MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
            };
          };
          db-sync = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env = {
              DB_NAME = "preprod_dbsync";
              DATA_DIR = "/persist/db-sync";
              ENVIRONMENT = "preprod";
              # CONSUL_KV_PATH = "config/cardano/mixed";
              # VAULT_KV_PATH = "kv/data/db-sync/mixed";
              MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.us-east-1.consul";
            };
          };
      };
    };
}
