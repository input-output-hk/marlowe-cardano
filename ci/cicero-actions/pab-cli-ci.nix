{ name, std, ... }@args:
let
  getStart = e: e."marlowe-cardano/ci".start;
in
{
  inputs = {
    marlowe-event = ''
      "marlowe-cardano/ci": start: {
        clone_url:     string
        sha:           string
        statuses_url?: string
      }
    '';
  };

  outputs = { marlowe-event }: {
    success = {
      ${name}.success = { inherit (getStart marlowe-event) clone_url sha; };
    };
    failure = {
      ${name}.failure = { inherit (getStart marlowe-event) clone_url sha; };
    };
  };

  job = { marlowe-event }:
    let
      start = getStart marlowe-event;
      flakeBase = "git+${start.clone_url}?rev=${start.sha}";
    in
    std.chain args [
      (std.escapeNames [ ] [ ])
      (name: task: {
        ${name}.group.${name} = {
          network = {
            port.node = { };
            port.pab = { };
          };
          task = {

            node = {
              resources.memory = 4096;
              resources.cpu = 2000;
              config = {
                packages = [
                  "${flakeBase}#node"
                ];

                environment.NODE_STATE_DIR = "/tmp/node";

                command = [ "/bin/entrypoint" ];
              };
            };
            chainIndex = {
              resources.memory = 2048;
              resources.cpu = 2000;

              config = {
                packages = [
                  "${flakeBase}#chain-index"
                ];
                command = [ "/bin/entrypoint" ];
                environment.INDEX_STATE_DIR = "/tmp/index";
              };
            };
            wbe = {
              resources.memory = 2048;
              resources.cpu = 2000;

              config = {
                packages = [
                  "${flakeBase}#wbe"
                ];
                command = [ "/bin/entrypoint" ];
              };
            };
            pab = {
              resources.memory = 2048;

              config = {
                packages = [
                  "${flakeBase}#marlowe-run-entrypoint"
                ];
                command = [ "/bin/entrypoint" ];
                environment.PAB_STATE_DIR = "/tmp/pab";
              };
            };
            ${name} = task;
          };
        };
      })

      {
        config.packages = std.data-merge.append [
          "${flakeBase}#wait-for-socket"
          "${flakeBase}#marlowe-cli"
          "${flakeBase}#print-test-dir"
        ];
      }

      (std.script "bash" ''
        set -eEuo pipefail

        MAGIC=(--testnet-magic 1564)
        CARDANO_NODE_SOCKET_PATH=$NOMAD_ALLOC_DIR/node.sock
        WALLET_API=$NOMAD_ADDR_wbe
        PAB_API=$NOMAD_ADDR_pab
        FAUCET_KEY="$TREASURY"/payment.skey
        FAUCET_ADDRESS=addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
        PAB_PASSPHRASE=fixme-allow-pass-per-wallet

        for t in $(print-test-dir)/test-{wait,refund,simple,escrow}.yaml
        do
          marlowe-cli test contracts "''${MAGIC[@]}"                           \
                                     --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                     --wallet-url "$WALLET_API"                \
                                     --pab-url "$PAB_API"                      \
                                     --faucet-key "$FAUCET_KEY"                \
                                     --faucet-address "$FAUCET_ADDRESS"        \
                                     --burn-address "$BURN_ADDRESS"            \
                                     --passphrase "$PAB_PASSPHRASE"            \
                                     $t                                        \
          | tee ''${t%%.yaml}.log
        done
      '')
    ]
      { };
}
