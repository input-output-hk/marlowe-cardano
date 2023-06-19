{ inputs, inputs', pkgs, project }:

{
  cabal-fmt.enable = true;
  cabal-fmt.extraOptions = "--no-tabular";


  nixpkgs-fmt.enable = true;


  shellcheck.enable = true;


  editorconfig-checker.enable = true;


  fourmolu.enable = true;
  fourmolu.extraOptions = "--ghc-opt -XTypeApplications";


  hlint.enable = true;
}
