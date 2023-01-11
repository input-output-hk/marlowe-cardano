{ inputs }:

let
  inherit (inputs) self data-merge dapps-world cardano-world nixpkgs bitte-cells;

  inherit (cardano-world) cardano;
  inherit (dapps-world) cloud;
  inherit (bitte-cells) vector;
  inherit (cloud.constants) baseDomain;
  inherit (data-merge) merge;

  inherit (self) nomadTasks;

  # marlowe namespace is created and configured in dapps-world

  mkRuntimeJob = environment:
    let
      jobname = "marlowe-runtime-${environment}";

      id = jobname;
      namespace = "marlowe";
      domain = "${jobname}.${baseDomain}";
      scaling = 1;

      datacenters = ["us-east-1" "eu-central-1" "eu-west-1"];
      type = "service";
      priority = 50;

      # Pull out cardano-node task to merge in with rest
      node' = (cardano.nomadCharts.cardano-node {
        inherit namespace datacenters domain scaling;
        jobname = "node";
        nodeClass = namespace;
      }).job.node.group.cardano;
      group = builtins.removeAttrs node' ["task"];
      node = group // {task.node = node'.task.node;};

    in
      {
        job.${jobname} = (import ./scheduling-config.nix) // {
          inherit namespace datacenters id type priority;

          group.marlowe-runtime =
            merge
            # task.vector ...
            (vector.nomadTask.default {
              inherit namespace;
              endpoints = [ ];
            })
            (
              merge node
                {
                  task = {
                    node = {
                      lifecycle.sidecar = true;
                      env = {
                        ENVIRONMENT = environment;
                        DATA_DIR = "/persist/${jobname}";
                        LOCAL_ROOTS_SRV_DNS = "_${jobname}-node._tcp.service.consul";
                        PUBLIC_ROOTS_SRV_DNS = "_${environment}-node._tcp.service.consul";
                      };
                    };
                    inherit (nomadTasks)
                      marlowe-history
                      marlowe-discovery
                      marlowe-tx;
                    chain-indexer = nomadTasks.chain-indexer { inherit environment; };
                    chainseekd = nomadTasks.chainseekd { inherit environment; };
                  };
                }
            );
        };
      };
in
{
  marlowe-runtime-preprod = mkRuntimeJob "preprod";
  marlowe-runtime-preview = mkRuntimeJob "preview";
  marlowe-runtime-mainnet = mkRuntimeJob "mainnet";
}
