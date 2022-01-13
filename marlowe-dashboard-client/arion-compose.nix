{ lib
, pkgs
, ...
}:
let
  inherit (pkgs) coreutils;
  inherit (pkgs.marlowe) cardano-node;
  node-port = "3001";
in
{
  config.services = {
    cardano-node = {
      image = {
        name = "cardano-node";
        contents = [ cardano-node ];
        command = [
          "${cardano-node}/bin/cardano-node"
          "run"
          "--config"
          "/var/config/config.json"
          "--topology"
          "/var/config/topology.yaml"
          "--database-path"
          "/var/db"
          "--socket-path"
          "/var/alloc/node.sock"
          "--port"
          node-port
        ];
      };
      service = {
        useHostStore = true;
        ports = [ "${node-port}:${node-port}" ];
        volumes = [
          "alloc-volume:/var/alloc"
          "${../bitte/node/config}:/var/config"
          "/var/db"
        ];
      };
    };
  };
  config.docker-compose.raw = {
    volumes = {
      alloc-volume = { };
    };
  };
}
