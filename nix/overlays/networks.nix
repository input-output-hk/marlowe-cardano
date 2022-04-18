# NOTE self == pkgs
self: _:
let
  inherit (self.iohkNix) cardanoLib;
  inherit (cardanoLib) environments mkEdgeTopology;
  mkNetwork = env: env // {
    topology = mkEdgeTopology {
      edgeNodes = [ env.relaysNew ];
      edgePort = env.edgePort;
      valency = 1;
    };
  };
in
{
  networks = {
    testnet-pioneers = mkNetwork environments.marlowe-pioneers;
    testnet-dev = mkNetwork environments.marlowe-dev;
    # TODO add testnet-public, mainnet in the future
  };
}
