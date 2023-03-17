{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) removePrefix mapAttrsToList mapAttrs;
  inherit (nixpkgs.lib.strings) concatMapStrings;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;

  mkImage = { name, description }: tag:
    std.lib.ops.mkStandardOCI {
      inherit tag;
      name = "ghcr.io/input-output-hk/${name}";
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

  forAllImages = f: tag: concatMapStrings
    (s: s + "\n")
    (mapAttrsToList (_: img: f (img tag)) images);

  mkImages = tag: (mapAttrs (_: img: img tag) images) // {
    all = {
      copyToDockerDaemon = std.lib.ops.writeScript {
        name = "copy-to-docker-daemon";
        text = forAllImages (img: "${img.copyToDockerDaemon}/bin/copy-to-docker-daemon") tag;
      };
      copyToRegistry = std.lib.ops.writeScript {
        name = "copy-to-registry";
        text = forAllImages (img: "${img.copyToRegistry}/bin/copy-to-registry") tag;
      };
    };
  };
in
{
  latest = mkImages "latest";
}
