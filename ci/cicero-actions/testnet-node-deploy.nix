{ name, std, actionLib, nixpkgsRev, lib, ... }@args: {
  inputs.start = ''
    "${name}": {
      sha: string
      ref?: string
      clone_url?: string
    }
  '';

  job = { start }:
    let
      cfg = start.value.${name};
      pkg = pkg: "github:NixOS/nixpkgs/${nixpkgsRev}#${pkg}";
      clone_url = cfg.clone_url or "https://github.com/input-output-hk/marlowe-cardano";
      flakeBase = "git+${clone_url}?rev=${cfg.sha}${if cfg ? ref then "&ref=${cfg.ref}" else ""}";
      spec.job.marlowe-testnet-node = {
        type = "service";
        datacenters = [
          "dc1"
          "eu-central-1"
          "us-east-2"
        ];
        namespace = "marlowe";
        group.testnet-node = {
          network = {
            mode = "host";
            port = {
              node-socat = { };
              node = { };
              wbe = { };
              index = { };
            };
          };

          count = 1;

          volume = {
            marlowe = {
              type = "host";
              source = "marlowe";
            };
          };

          task = {
            node-socat = {
              driver = "exec";

              resources = {
                memory = 1024;
              };

              config = {
                flake = "${flakeBase}#node-socat";

                command = [ "/bin/node-socat" ];
              };

              service = {
                port = "node-socat";
                tags = [ "marlowe-testnet" ];
              };
            };

            node = let state-dir = "/var/lib/marlowe/private-testnet/cardano-node"; in
              {
                driver = "exec";

                resources.memory = 4096;

                resources.cpu = 2000;

                volume_mount = {
                  volume = "marlowe";
                  destination = dirOf (dirOf state-dir);
                };

                config = {
                  flake = "${flakeBase}#node";

                  command = "/bin/entrypoint";
                };

                env.NODE_STATE_DIR = state-dir;

                service = {
                  port = "node";
                  tags = [ "marlowe-testnet" ];
                };
              };

            wbe = {
              driver = "nix";

              resources = {
                memory = 2048;

                cpu = 2000;
              };

              config = {
                packages = [ "${flakeBase}#wbe" ];

                command = [ "/bin/entrypoint" ];
              };

              service = {
                port = "wbe";
                tags = [ "marlowe-testnet" ];
              };
            };

            chain-index = let state-dir = "/var/lib/marlowe/private-testnet/chain-index"; in
              {
                driver = "nix";

                resources = {
                  memory = 2048;

                  cpu = 2000;
                };

                volume_mount = {
                  volume = "marlowe";

                  destination = dirOf (dirOf state-dir);
                };

                config = {
                  packages = [ "${flakeBase}#chain-index" ];

                  command = [ "/bin/entrypoint" ];
                };

                env.INDEX_STATE_DIR = state-dir;

                service = {
                  port = "index";
                  tags = [ "marlowe-testnet" ];
                };
              };
          };
        };
      };
    in
    std.chain args [
      actionLib.simpleJob

      {
        resources.memory = 1024;
        config.packages = std.data-merge.append (map pkg [ "nomad" "cacert" ]);
      }

      (std.script "bash" ''
        export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
        echo ${lib.escapeShellArg (builtins.toJSON spec)} > testnet-node.json
        nomad run testnet-node.json
      '')
    ];
}
