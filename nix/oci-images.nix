{ inputs }:
let
  inherit (inputs) std self;
  inherit (self) operables;

  mkImage = name: std.lib.ops.mkStandardOCI {
    name = "registry.ci.iog.io/${name}";
    operable = operables.${name};
    debug = true;
  };

in {
  chain-indexer = mkImage "chain-indexer";
  chainseekd = mkImage "chainseekd";
  marlowe-history = mkImage "marlowe-history";
  marlowe-discovery = mkImage "marlowe-discovery";
  marlowe-tx = mkImage "marlowe-tx";
}
