{
  description = "Marlowe Cardano implementation";


  inputs = {

    std = {
      url = "github:divnix/std";
      inputs.n2c.follows = "n2c";
    };

    # Use upstream when https://github.com/nlewo/nix2container/pull/82 is merged
    n2c.url = "github:shlevy/nix2container/no-Size-on-dir";

    marlowe-plutus.url = "github:input-output-hk/marlowe-plutus";

    cardano-node.url = "github:input-output-hk/cardano-node?ref=8.1.2";

    cardano-world.url = "github:input-output-hk/cardano-world/d22f50fc77d23e2612ca2b313a098dd0b48834d4";

    iogx = {
      url = "github:input-output-hk/iogx?ref=v4";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-linux" "x86_64-darwin" ];
    outputs = import ./nix/outputs.nix;
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
