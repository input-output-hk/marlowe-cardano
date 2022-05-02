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
    testnet-pioneers = mkNetwork environments.marlowe-pioneers // {
      slotZeroTime = 1649949631000;
      slotLengthMillis = 1000;
      magic = 1567;
    };
    testnet-dev = mkNetwork environments.marlowe-dev // {
      slotZeroTime = 1649976791000;
      slotLengthMillis = 1000;
      magic = 1566;
    };
    # TODO add testnet-public, mainnet in the future
  };
}
