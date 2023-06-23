{ inputs, inputs', pkgs, project }:

{
  cabal-fmt.enable = true;
  cabal-fmt.extraOptions = "--no-tabular";


  nixpkgs-fmt.enable = true;


  shellcheck.enable = true;


  fourmolu.enable = true;


  hlint.enable = true;
}
