{ pkgs
, gitignore-nix
, fixStylishHaskell
, fix-purs-tidy
, fix-prettier
, fixPngOptimization
, src
, marlowe-playground
, marlowe-dashboard
, web-ghc
, plutus-pab
, marlowe-pab
, docs
, sources
, vmCompileTests ? false
}:
let
  inherit (pkgs) lib;
  cleanSrc = gitignore-nix.gitignoreSource src;
in
pkgs.recurseIntoAttrs {
  shellcheck = pkgs.callPackage ./shellcheck.nix { src = cleanSrc; };

  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    src = cleanSrc;
    inherit fixStylishHaskell;
  };

  pursTidy = pkgs.callPackage ./purs-tidy.nix {
    src = cleanSrc;
    inherit fix-purs-tidy;
  };

  prettier = pkgs.callPackage ./prettier.nix {
    src = cleanSrc;
    inherit fix-prettier;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    src = cleanSrc;
    inherit (pkgs) nixpkgs-fmt;
  };

  pngOptimization = pkgs.callPackage ./png-optimization.nix {
    src = cleanSrc;
    inherit fixPngOptimization;
  };

  vmTests = pkgs.callPackage ./vm.nix {
    inherit vmCompileTests marlowe-playground
      marlowe-dashboard web-ghc plutus-pab marlowe-pab docs sources;
  };
}
