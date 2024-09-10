{ repoRoot, inputs, pkgs, lib, system }:

# TODO Remove this file once cardano-node can build aarch64-darwin.

{
  cardano-node =
    if system == "aarch64-darwin" then
      inputs.cardano-node.packages.x86_64-darwin.cardano-node
    else if system == "aarch64-linux" then
      inputs.cardano-node.packages.x86_64-linux.cardano-node
    else
      inputs.cardano-node.packages.${system}.cardano-node;

  cardano-cli =
    if system == "aarch64-darwin" then
      inputs.cardano-cli.packages.x86_64-darwin."cardano-cli:exe:cardano-cli"
    else if system == "aarch64-linux" then
      inputs.cardano-cli.packages.x86_64-linux."cardano-cli:exe:cardano-cli"
    else
      inputs.cardano-cli.packages.${system}."cardano-cli:exe:cardano-cli";

  cardano-addresses =
    if system == "aarch64-darwin" then
      inputs.cardano-addresses.packages.x86_64-darwin."cardano-addresses-cli:exe:cardano-address"
    else if system == "aarch64-linux" then
      inputs.cardano-addresses.packages.x86_64-linux."cardano-addresses-cli:exe:cardano-address"
    else
      inputs.cardano-addresses.packages.${system}."cardano-addresses-cli:exe:cardano-address";

}
