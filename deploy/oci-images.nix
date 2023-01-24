{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) removePrefix;
  inherit (self) operables;
  inherit (self.sourceInfo) lastModifiedDate;

  mkImage = name: std.lib.ops.mkStandardOCI {
    name = "registry.ci.iog.io/dapps-world-${name}";
    operable = operables.${name};
    debug = true;
  };

  mkPublicImage = name:
    let
      tagName = removePrefix "marlowe-" name;
      tagDate = builtins.substring 0 8 lastModifiedDate; # pull out just date
    in
    std.lib.ops.mkStandardOCI {
      name = "iohkbuild/marlowe";
      tag = "${tagName}-${tagDate}";
      operable = operables.${name};
    };
in
{
  chain-indexer = mkImage "chain-indexer";
  chainseekd = mkImage "chainseekd";
  marlowe-history = mkImage "marlowe-history";
  marlowe-discovery = mkImage "marlowe-discovery";
  marlowe-tx = mkImage "marlowe-tx";

  chain-indexer-public = mkPublicImage "chain-indexer";
  chainseekd-public = mkPublicImage "chainseekd";
  marlowe-history-public = mkPublicImage "marlowe-history";
  marlowe-discovery-public = mkPublicImage "marlowe-discovery";
  marlowe-tx-public = mkPublicImage "marlowe-tx";
}
