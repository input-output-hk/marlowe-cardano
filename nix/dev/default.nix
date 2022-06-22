{ pkgs
, network
, marlowe-dashboard
, marlowe-dashboard-client
, cardano-cli
, marlowe-pab
, cardano-node
, cardano-wallet
, plutus-chain-index
, marlowe-playground
, web-ghc-server
, ghcWithPackages
}:
let
  marlowe-pab-exe = marlowe-pab + "/bin/marlowe-pab";
  marlowe-dashboard-exe = marlowe-dashboard + "/bin/marlowe-dashboard-server";
  web-ghc-ghc = ghcWithPackages (ps: [
    ps.playground-common
    ps.plutus-core
    ps.plutus-tx
    ps.plutus-contract
    ps.plutus-ledger
    ps.marlowe
  ]);

  devNetworkConfig = rec {
    node = {
      config-file = pkgs.writeTextFile {
        name = "node-config.json";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/node-config.nix { config = network.nodeConfig; });
      };
      port = 3001;
      socket-path = "/tmp/node.socket";
      database-path = "db/node.db";
    };
    wallet = {
      testnet = network.nodeConfig.ByronGenesisFile;
      database-path = "db/wallet.db";
      port = 8090;
    };
    chain-index = {
      database-path = "db/chain-index.db";
      port = 9083;
    };
    web-ghc = {
      port = 8091;
    };
    playground = {
      port = 8092;
    };
    pab = {
      port = 9080;
      config-params = {
        baseUrl = "http://localhost:${toString pab.port}";
        walletUrl = "http://localhost:${toString wallet.port}";
        chainIndexUrl = "http://localhost:${toString chain-index.port}";
        socket-path = node.socket-path;
        inherit network;
        protocol-parameters = "/tmp/testnet.protocol";
      };
      config-file = pkgs.writeTextFile {
        name = "marlowe-pab.yaml";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/pab-config.nix pab.config-params);
      };
    };

    dashboard-server = {
      config-params = {
        chain-index-port = chain-index.port;
        chain-index-host = "localhost";
        wallet-port = wallet.port;
        wallet-host = "localhost";
      };
      config-file = pkgs.writeTextFile {
        name = "dashboard-server.json";
        text = builtins.toJSON (import ../../marlowe-dashboard-client/dev/marlowe-run.nix dashboard-server.config-params);
      };
      port = 8080;
    };
    topology = network.topology;
  };

  start-cardano-node = writeShellScriptBinInRepoRoot "start-cardano-node" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"
    mkdir -p ${devNetworkConfig.node.database-path}
    cardano-node run \
            --config ${devNetworkConfig.node.config-file} \
            --topology ${devNetworkConfig.topology} \
            --port ${toString devNetworkConfig.node.port} \
            --socket-path ${devNetworkConfig.node.socket-path} \
            --database-path ${devNetworkConfig.node.database-path}
  '';

  start-wallet = writeShellScriptBinInRepoRoot "start-cardano-wallet" ''
    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    mkdir -p ${devNetworkConfig.wallet.database-path}

    cardano-wallet serve \
      --testnet ${devNetworkConfig.wallet.testnet} \
      --database ${devNetworkConfig.wallet.database-path} \
      --node-socket ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.wallet.port}
  '';

  start-chain-index = writeShellScriptBinInRepoRoot "start-chain-index" ''
    mkdir -p ${devNetworkConfig.chain-index.database-path}

    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    ${plutus-chain-index}/bin/plutus-chain-index start-index \
      --network-id ${toString network.magic} \
      --db-path ${devNetworkConfig.chain-index.database-path}/ci.sqlite \
      --socket-path ${devNetworkConfig.node.socket-path} \
      --port ${toString devNetworkConfig.chain-index.port}

  '';

  start-marlowe-pab = writeShellScriptBinInRepoRoot "start-marlowe-pab" ''
    echo "socket path = ${devNetworkConfig.node.socket-path}"

    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"

    CARDANO_NODE_SOCKET_PATH=${devNetworkConfig.node.socket-path} ${cardano-cli}/bin/cardano-cli query \
            protocol-parameters \
            --testnet-magic ${toString network.magic} \
            --out-file ${devNetworkConfig.pab.config-params.protocol-parameters}

    ${marlowe-pab-exe} webserver \
            --config ${devNetworkConfig.pab.config-file} \
            --passphrase fixme-allow-pass-per-wallet \
            --memory
  '';

  start-dashboard-server = writeShellScriptBinInRepoRoot "start-dashboard-server" ''
    echo "Waiting for cardano-node socket connection"
    until ${pkgs.socat}/bin/socat /dev/null UNIX-CONNECT:${devNetworkConfig.node.socket-path} 2> /dev/null; do :; done
    echo "Connection ready"


    ${marlowe-dashboard-exe} webserver \
      --config ${devNetworkConfig.dashboard-server.config-file} \
      --port ${toString devNetworkConfig.dashboard-server.port} \
      --network-id ${toString network.magic} \
      --verbosity 2
  '';

  start-marlowe-run = writeShellScriptBinInRepoRoot "start-marlowe-run" ''
    #!/bin/bash

    # The spago dependencies might fail downloading, so we invoke it until it works
    cd marlowe-dashboard-client
    counter=5
    spago install
    while [[ "$?" -ne 0 && "$counter" -gt 0 ]]; do
      let "counter-=1";
      echo "Failed, retrying $counter more times";
      spago install
    done
    cd ..

    ${pkgs.tmux}/bin/tmux -T 256,mouse,focus,title\
      new-session "printf '\033]2;Cardano node\033\\' && start-cardano-node" \; \
      set mouse on \; \
      set pane-border-status top \; \
      set pane-border-format "#{pane_index} #T" \; \
      setw remain-on-exit on \; \
      split-window -h "printf '\033]2;PAB\033\\' && start-marlowe-pab" \; \
      split-window -h "printf '\033]2;WBE\033\\' && start-cardano-wallet" \; \
      split-window "printf '\033]2;Chain IX\033\\' && start-chain-index" \; \
      split-window "printf '\033]2;MRun BE\033\\' && start-dashboard-server" \; \
      split-window "printf '\033]2;MRun FE\033\\' && cd marlowe-dashboard-client && npm run start" \; \
      rename-window "Marlowe Run" \;
  '';

  #
  # dev convenience scripts
  #
  writeShellScriptBinInRepoRoot = name: script: pkgs.writeShellScriptBin name ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    ${script}
  '';

  run-with-node-sock = pkgs.writeShellScriptBin "run-with-node-sock" ''
    set -eEuo pipefail
    ${pkgs.wait-for-socket}/bin/wait-for-socket /shared/node.sock
    exec "$@"
  '';

  run-with-protocol-parameters = pkgs.writeShellScriptBin "run-with-protocol-parameters" ''
    set -eEuo pipefail
    CARDANO_NODE_SOCKET_PATH=/shared/node.sock ${cardano-cli}/bin/cardano-cli \
      query protocol-parameters \
      --testnet-magic ${toString network.magic} \
      --out-file /protocol-params
    exec "$@"
  '';

  docker-scripts = args@{ name, config, tag ? "latest", ... }:
    let
      full-name = "inputoutput/marlowe-${name}";
      stream = pkgs.dockerTools.streamLayeredImage (args // {
        inherit tag;

        name = full-name;
      });
    in
    {
      load = pkgs.writeShellScriptBin "load-docker-${name}" ''
        ${stream} | docker load
      '';

      upload = pkgs.writeShellScriptBin "upload-docker-${name}" ''
        set -eEuo pipefail

        PATH="${pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.skopeo pkgs.gzip ]}"

        if [ "$#" -ne "1" ]
        then
          echo "Usage: $0 IMAGE_URL # skopeo format" >&2
          exit 1
        fi
        image="$1"

        ${stream} | gzip --fast | skopeo --insecure-policy copy docker-archive:/dev/stdin $image
      '';

      inherit config tag full-name;
    };

  inherit (pkgs.lib) mapAttrs optionalAttrs;
  inherit (pkgs) iana-etc coreutils lighttpd writeText z3 cacert;
in
{
  scripts = { inherit start-cardano-node start-wallet start-chain-index start-marlowe-pab start-dashboard-server start-marlowe-run; };
  docker = rec {
    images = mapAttrs (name: args: docker-scripts ({ inherit name; } // args)) {
      node = {
        contents = [ cardano-node iana-etc ];
        config = {
          CMD = [
            "cardano-node"
            "run"
            "--config"
            devNetworkConfig.node.config-file
            "--topology"
            devNetworkConfig.topology
            "--port"
            (toString devNetworkConfig.node.port)
            "--socket-path"
            "/shared/node.sock"
            "--database-path"
            "/db"
          ];
          EXPOSE = devNetworkConfig.node.port;
          VOLUME = [ "/shared" "/db" ];
        };
      };

      chain-index = {
        contents = [ plutus-chain-index run-with-node-sock iana-etc ];
        config = {
          CMD = [
            "run-with-node-sock"
            "plutus-chain-index"
            "start-index"
            "--network-id"
            (toString network.magic)
            "--db-path"
            "/db/index.sqlite"
            "--socket-path"
            "/shared/node.sock"
            "--port"
            (toString devNetworkConfig.chain-index.port)
          ];
          EXPOSE = devNetworkConfig.chain-index.port;
          VOLUME = [ "/shared" "/db" ];
        };
      };

      wallet = {
        contents = [ cardano-wallet run-with-node-sock iana-etc ];
        config = {
          CMD = [
            "run-with-node-sock"
            "cardano-wallet"
            "serve"
            "--testnet"
            devNetworkConfig.wallet.testnet
            "--database"
            "/db"
            "--node-socket"
            "/shared/node.sock"
            "--listen-address"
            "*"
            "--port"
            (toString devNetworkConfig.wallet.port)
          ];
          EXPOSE = devNetworkConfig.wallet.port;
          VOLUME = [ "/shared" "/db" ];
        };
      };

      pab = {
        contents = [ marlowe-pab run-with-node-sock run-with-protocol-parameters iana-etc ];
        config = {
          CMD = [
            "run-with-node-sock"
            "run-with-protocol-parameters"
            "marlowe-pab"
            "webserver"
            "--config"
            (writeText "marlowe-pab.yaml" (builtins.toJSON (import ../../marlowe-dashboard-client/dev/pab-config.nix {
              baseUrl = "http://localhost:${toString devNetworkConfig.pab.port}";
              walletUrl = "http://wallet:${toString devNetworkConfig.wallet.port}";
              chainIndexUrl = "http://chain-index:${toString devNetworkConfig.chain-index.port}";
              socket-path = "/shared/node.sock";
              inherit network;
              protocol-parameters = "/protocol-params";
            })))
            "--passphrase"
            "fixme-allow-pass-per-wallet"
            "--memory"
          ];
          EXPOSE = devNetworkConfig.pab.port;
          VOLUME = [ "/shared" ];
        };
      };

      dashboard-server = {
        contents = [ marlowe-dashboard run-with-node-sock iana-etc ];
        config = {
          CMD = [
            "run-with-node-sock"
            "marlowe-dashboard-server"
            "webserver"
            "--config"
            (writeText "dashboard-server.json" (builtins.toJSON (import ../../marlowe-dashboard-client/dev/marlowe-run.nix {
              chain-index-port = devNetworkConfig.chain-index.port;
              chain-index-host = "chain-index";
              wallet-port = devNetworkConfig.wallet.port;
              wallet-host = "wallet";
            })))
            "--bind"
            "0.0.0.0"
            "--port"
            (toString devNetworkConfig.dashboard-server.port)
            "--network-id"
            (toString network.magic)
            "--verbosity"
            "2"
          ];
          EXPOSE = devNetworkConfig.dashboard-server.port;
          VOLUME = [ "/shared" ];
        };
      };

      web-ghc = {
        contents = [ web-ghc-server iana-etc ];
        config = {
          CMD = [
            "web-ghc-server"
            "webserver"
            "--port"
            (toString devNetworkConfig.web-ghc.port)
            "--bind"
            "0.0.0.0"
          ];
          EXPOSE = devNetworkConfig.web-ghc.port;
          ENV = [
            "GHC_LIB_DIR=${web-ghc-ghc}/lib/ghc-${web-ghc-ghc.version}"
            "GHC_BIN_DIR=${web-ghc-ghc}/bin"
            "GHC_PACKAGE_PATH=${web-ghc-ghc}/lib/ghc-${web-ghc-ghc.version}/package.conf.d"
            "GHC_RTS=-M2G"
          ];
        };
        extraCommands = "mkdir tmp";
      };

      playground-server = {
        contents = [ marlowe-playground.server z3 iana-etc cacert ];
        config = {
          CMD = [
            "marlowe-playground-server"
            "webserver"
            "-p"
            (toString devNetworkConfig.playground.port)
          ];
          EXPOSE = devNetworkConfig.playground.port;
          ENV = [ "WEBGHC_URL=web-ghc:${toString devNetworkConfig.web-ghc.port}" ];
        };
      };

      playground = {
        contents = [ lighttpd iana-etc ];
        config = {
          CMD = [
            "lighttpd"
            "-f"
            (writeText "lighttpd.conf" ''
              server.modules = ( "mod_deflate", "mod_proxy" )
              server.document-root = "${marlowe-playground.client}"
              server.port = 80
              index-file.names = ("index.html")
              mimetype.assign = (
                ".css"  => "text/css",
                ".jpg"  => "image/jpeg",
                ".jpeg" => "image/jpeg",
                ".html" => "text/html",
                ".js"   => "text/javascript",
                ".svg"  => "image/svg+xml",
              )
              deflate.cache-dir = "/tmp"
              deflate.mimetypes = ("text/plain", "text/html", "text/css")
              server.upload-dirs = ("/tmp")
              $HTTP["url"] =~ "^/api" {
                proxy.server = ("" => (( "host" => "playground-server", "port" => "${toString devNetworkConfig.playground.port}" )))
              }
            '')
            "-D"
          ];
          EXPOSE = 80;
        };
      };

      run = {
        contents = [ lighttpd iana-etc ];
        config = {
          CMD = [
            "lighttpd"
            "-f"
            (writeText "lighttpd.conf" ''
              server.modules = ( "mod_deflate", "mod_proxy" )
              server.document-root = "${marlowe-dashboard-client}"
              server.port = 80
              index-file.names = ("index.html")
              mimetype.assign = (
                ".css"  => "text/css",
                ".jpg"  => "image/jpeg",
                ".jpeg" => "image/jpeg",
                ".html" => "text/html",
                ".js"   => "text/javascript",
                ".svg"  => "image/svg+xml",
              )
              deflate.cache-dir = "/tmp"
              deflate.mimetypes = ("text/plain", "text/html", "text/css")
              server.upload-dirs = ("/tmp")
              $HTTP["url"] =~ "^/pab" {
                proxy.server = ("" => (( "host" => "pab", "port" => "${toString devNetworkConfig.pab.port}" )))
                proxy.header = ( "upgrade" => "enable", "map-urlpath" => ("/pab/" => "/") )
              }
              $HTTP["url"] =~ "^/api" {
                proxy.server = ("" => (( "host" => "dashboard-server", "port" => "${toString devNetworkConfig.dashboard-server.port}" )))
              }
            '')
            "-D"
          ];
          EXPOSE = 80;
        };
      };
    };

    compose = writeText "compose.yaml" (builtins.toJSON {
      services = mapAttrs
        (name: image: {
          image = "${image.full-name}:${image.tag}";

          volumes = map (vol: if vol == "/shared" then "shared:/shared" else "${name}-db:${vol}") (image.config.VOLUME or [ ]);
        } // optionalAttrs (builtins.elem name [ "playground" "run" ]) {
          ports = [ 80 ];
        })
        images;
      volumes = {
        shared = { };
        wallet-db = { };
        chain-index-db = { };
        node-db = { };
      };
    });
  };
}
