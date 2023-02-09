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
  # ensure chain-indexer and chainseekd have access to node socket
  chain-indexer = mkImage "chain-indexer" rootConfig;
  chainseekd = mkImage "chainseekd" rootConfig;
  marlowe-history = mkImage "marlowe-history" { };
  marlowe-discovery = mkImage "marlowe-discovery" { };
  marlowe-tx = mkImage "marlowe-tx" { };
}
