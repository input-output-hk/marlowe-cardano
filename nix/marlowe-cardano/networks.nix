{ inputs, pkgs, ... }:

let
  inherit (pkgs.cardanoLib) environments mkEdgeTopology;
  mkNetwork = env: env // {
    topology = mkEdgeTopology {
      edgeNodes = [ env.relaysNew ];
      edgePort = env ? edgePort;
      valency = 1;
    };
  };
in
{
  testnet = mkNetwork environments.testnet // {
    magic = 1097911063;
  };
  preview = mkNetwork environments.preview // {
    name = "preview";
    magic = 2;
  };
  preprod = mkNetwork environments.preprod // {
    name = "preprod";
    magic = 1;
  };
  # TODO add testnet-public, mainnet in the future
}
