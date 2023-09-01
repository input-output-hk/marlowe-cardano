# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{
  cabal-fmt.enable = true;
  cabal-fmt.extraOptions = "--no-tabular";


  nixpkgs-fmt.enable = true;


  shellcheck.enable = true;


  fourmolu.enable = true;


  hlint.enable = true;
}
