{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) removePrefix mapAttrsToList;
  inherit (nixpkgs.lib.strings) concatMapStrings;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;

  mkImage = name:
    let
      tagName = removePrefix "marlowe-" name;
      tagDate = builtins.substring 0 8 lastModifiedDate; # pull out just date
    in
    std.lib.ops.mkStandardOCI {
      name = "iohkbuild/marlowe";
      tag = "${tagName}-${tagDate}";
      operable = operables.${name};
      uid = "0";
      gid = "0";
    };

  images = {
    chain-indexer = mkImage "chain-indexer";
    marlowe-chain-sync = mkImage "marlowe-chain-sync";
    marlowe-indexer = mkImage "marlowe-indexer";
    marlowe-sync = mkImage "marlowe-sync";
    marlowe-tx = mkImage "marlowe-tx";
    marlowe-web-server = mkImage "marlowe-web-server";
  };

  forAllImages = f: concatMapStrings (s: s + "\n") (mapAttrsToList (_: f) images);
in
images // {

  all = {
    copyToDockerDaemon = std.lib.ops.writeScript {
      name = "copy-to-docker-daemon";
      text = forAllImages (img: "${img.copyToDockerDaemon}/bin/copy-to-docker-daemon");
    };
    copyToRegistry = std.lib.ops.writeScript {
      name = "copy-to-registry";
      text = forAllImages (img: "${img.copyToRegistry}/bin/copy-to-registry");
    };
  };
}
