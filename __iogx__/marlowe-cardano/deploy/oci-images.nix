{ inputs, pkgs }:
let
  inherit (inputs) self std;
  inherit (pkgs.lib) removePrefix mapAttrsToList mapAttrs;
  inherit (pkgs.lib.strings) concatMapStrings;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;

  mkImage = { name, description }:
    std.lib.ops.mkStandardOCI {
      inherit name;
      tag = "latest";
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
    marlowe-proxy = mkImage {
      name = "marlowe-proxy";
      description = "An API Gateway service for the Marlowe Runtime.";
    };
    marlowe-web-server = mkImage {
      name = "marlowe-web-server";
      description = "An HTTP server for the Marlowe Runtime, exposing a REST API.";
    };
  };

  forAllImages = f: concatMapStrings (s: s + "\n") (mapAttrsToList (_: f) images);
in
images // {
  all = {
    copyToDockerDaemon = std.lib.ops.writeScript {
      name = "copy-to-docker-daemon";
      text = forAllImages (img: "${img.copyToDockerDaemon}/bin/copy-to-docker-daemon");
    };
  };
}
