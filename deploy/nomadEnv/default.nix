{ inputs }:

let
  inherit (inputs) self data-merge dapps-world cardano-world nixpkgs bitte-cells;

  inherit (cardano-world) cardano;
  inherit (dapps-world) cloud;
  inherit (bitte-cells) vector;
  inherit (cloud.constants) baseDomain;
  inherit (data-merge) merge append;
  inherit (nixpkgs.lib) genAttrs head splitString concatStringsSep toUpper replaceStrings;

  inherit (self) nomadTasks;

  # ports to configure for each task
  servicePorts = [
    "chainseekd"
    "chainseekd_query"
    "chainseekd_command"
    "history"
    "history_query"
    "history_sync"
    "discovery"
    "discovery_query"
    "discovery_sync"
    "tx"
  ];

  # environments to configure the runtime for
  environments = [
    "preprod"
    "preview"
    "mainnet"
  ];

  taskFromPort = p: head (splitString "_" p);
  formatService = replaceStrings [ "_" ] [ "-" ];


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
            # https://github.com/input-output-hk/bitte-cells/blob/main/cells/vector/nomadTask.nix
            (vector.nomadTask.default {
              inherit namespace;
              endpoints = [ ];
            })
            (
              merge node
                {
                  network.port =
                    (genAttrs servicePorts (n: {}))
                    // { ssh.to = 22; };
                  # Setup a service for each port, so that the sshd task can reference them
                  service = append (map (port: {
                    inherit port;
                    name = "\${JOB}-\${TASKGROUP}-${formatService port}";
                    task = taskFromPort port;
                  }) servicePorts);
                  meta = {
                    inherit environment;
                  };
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
                      chain-indexer
                      chainseekd
                      marlowe-history
                      marlowe-discovery
                      marlowe-tx;
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
