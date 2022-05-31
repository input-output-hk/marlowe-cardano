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
      pabResumeFrom = { tag = "PointAtGenesis"; };
      magic = 1567;
    };
    testnet-dev = mkNetwork environments.marlowe-dev // {
      slotZeroTime = 1649976791000;
      slotLengthMillis = 1000;
      pabResumeFrom = {
        tag = "Point";
        pointBlockId = "8d10922f8368d5426e1851b118e3f626725167593c04ccd28b0012d490ec8768";
        pointSlot = { getSlot = 3999983; };
      };
      magic = 1566;
    };
    # TODO add testnet-public, mainnet in the future
  };
}
