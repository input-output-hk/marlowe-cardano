{ inputs, pkgs, lib, ... }:

let
  inherit (inputs) self std n2c;
  inherit (lib) removePrefix mapAttrsToList mapAttrs;
  inherit (lib.strings) concatMapStrings;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;
  tag = "latest";
  mkImage = { name, description }:
    std.lib.ops.mkStandardOCI {
      inherit name;
      tag = tag;
      operable = operables.${name};
      uid = "0";
      gid = "0";
      labels = {
        inherit description;
        source = "https://github.com/input-output-hk/marlowe-cardano";
        license = "Apache-2.0";
      };
    };

  images = {
    marlowe-chain-indexer = mkImage {
      name = "marlowe-chain-indexer";
      description = "A Cardano chain indexer for the Marlowe Runtime";
    };
    marlowe-chain-sync = mkImage {
      name = "marlowe-chain-sync";
      description = "A Cardano chain sync and query service for the Marlowe Runtime.";
    };
    marlowe-indexer = mkImage {
      name = "marlowe-indexer";
      description = "A Marlowe contract indexing service for the Marlowe Runtime.";
    };
    marlowe-sync = mkImage {
      name = "marlowe-sync";
      description = "A Marlowe contract synchronization and query service for the Marlowe Runtime.";
    };
    marlowe-tx = mkImage {
      name = "marlowe-tx";
      description = "A Marlowe transaction creation service for the Marlowe Runtime.";
    };
    marlowe-contract = mkImage {
      name = "marlowe-contract";
      description = "A content-addressable contract store service for the Marlowe runtime.";
    };
    marlowe-proxy = mkImage {
      name = "marlowe-proxy";
      description = "An API Gateway service for the Marlowe Runtime.";
    };
    marlowe-runtime = mkImage {
      name = "marlowe-runtime";
      description = "Application backend for Marlowe smart contracts.";
    };
    marlowe-web-server = mkImage {
      name = "marlowe-web-server";
      description = "An HTTP server for the Marlowe Runtime, exposing a REST API.";
    };
    marlowe-benchmark = mkImage {
      name = "marlowe-benchmark";
      description = "A benchmarking tool for Marlowe Runtime.";
    };
    marlowe-pipe = mkImage {
      name = "marlowe-pipe";
      description = "A streaming command processor for the Marlowe Runtime.";
    };
    marlowe-oracle = mkImage {
      name = "marlowe-oracle";
      description = "A general-purpose oracle for Marlowe contracts.";
    };
  };

  forAllImages = f: concatMapStrings (s: s + "\n") (mapAttrsToList f images);
in
images // {
  all = {
    copyToDockerDaemon = std.lib.ops.writeScript {
      name = "copy-to-docker-daemon";
      text = forAllImages (name: image:
        ''echo ${name}; 
         echo ${image};
         echo ${n2c.packages.skopeo-nix2container}; 
         ${n2c.packages.skopeo-nix2container}/bin/skopeo  --insecure-policy copy nix:${image} docker-daemon:${name}:${tag}
         echo "Done!";
         ''
      );
    };
  };
}
