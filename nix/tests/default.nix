{ pkgs
, fixStylishHaskell
, fix-prettier
, src
, docs
, vmCompileTests ? false
}:
let
  inherit (pkgs) lib;
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage ./shellcheck.nix { inherit src; };

  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    inherit src fixStylishHaskell;
  };

  prettier = pkgs.callPackage ./prettier.nix {
    inherit src fix-prettier;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    inherit src;
    inherit (pkgs) nixpkgs-fmt;
  };
}
