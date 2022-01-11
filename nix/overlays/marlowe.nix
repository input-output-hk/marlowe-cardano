prev: final: {
  marloweProject = final.haskell-nix.project' {
    src = ../../marlowe;
    projectFileName = "stack.project";
    compiler-nix-name = "ghc8107";
    shell.tools = {
      cabal = {};
      hlint = {};
      haskell-language-server = {};
    };
    shell.buildInputs = with prev; [
      nixpkgs-fmt
    ];
  };
}

