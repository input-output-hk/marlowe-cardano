# NOTE self == pkgs
self: _:
let
  inherit (self.iohkNix) cardanoLib;
  inherit (cardanoLib) environments mkEdgeTopology;
  mkNetwork = env: env // {
    topology = mkEdgeTopology {
      edgeNodes = [ env.relaysNew ];
      edgePort = env?edgePort;
      valency = 1;
    };
  };
in
{
  networks = {
    testnet-pioneers = mkNetwork environments.marlowe-pioneers // {
      magic = 1567;
    };
    testnet-dev = mkNetwork environments.marlowe-dev // {
      magic = 1566;
    };
    testnet = mkNetwork environments.testnet // {
      magic = 1097911063;
    };
    preview = mkNetwork self.cardano.environments.preview // {
      name = "preview";
      magic = 2;
    };
    preprod = mkNetwork self.cardano.environments.preprod // {
      name = "preprod";
      magic = 1;
    };
    # TODO add testnet-public, mainnet in the future
  };
}
