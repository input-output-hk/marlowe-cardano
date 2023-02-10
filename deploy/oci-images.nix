{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) removePrefix;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;

  mkImage = name: args:
    let
      tagName = removePrefix "marlowe-" name;
      tagDate = builtins.substring 0 8 lastModifiedDate; # pull out just date
    in
    std.lib.ops.mkStandardOCI
      ({
        name = "iohkbuild/marlowe";
        tag = "${tagName}-${tagDate}";
        operable = operables.${name};
      }) // args;

  rootConfig = {
    uid = "0";
    gid = "0";
  };
in
{
  # ensure chain-indexer and marlowe-chain-sync have access to node socket
  chain-indexer = mkImage "chain-indexer" rootConfig;
  marlowe-chain-sync = mkImage "marlowe-chain-sync" rootConfig;
  marlowe-indexer = mkImage "marlowe-indexer" { };
  marlowe-sync = mkImage "marlowe-sync" { };
  marlowe-tx = mkImage "marlowe-tx" { };
}
