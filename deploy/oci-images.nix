{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) removePrefix;
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
in
{
  chain-indexer = mkImage "chain-indexer";
  marlowe-chain-sync = mkImage "marlowe-chain-sync";
  marlowe-indexer = mkImage "marlowe-indexer";
  marlowe-sync = mkImage "marlowe-sync";
  marlowe-tx = mkImage "marlowe-tx";
  marlowe-web-server = mkImage "marlowe-web-server";
}
