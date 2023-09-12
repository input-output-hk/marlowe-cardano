{ pkgs, ... }:

let
  project = pkgs.haskell-nix.hackage-project {
    name = "hspec-golden";
    version = "0.2.1.0";
    compiler-nix-name = "ghc8107";
  };

in

project.hsPkgs.hspec-golden.components.exes.hgold
