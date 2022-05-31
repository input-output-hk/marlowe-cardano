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
        pointBlockId = "a8f4a88d4387c24102529d14dd1b15037c0f38a864cbd58c72884948590415a3";
        pointSlot = { getSlot = 3290798; };
      };
      magic = 1566;
    };
    # TODO add testnet-public, mainnet in the future
  };
}
