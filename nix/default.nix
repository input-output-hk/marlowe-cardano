{ system
, crossSystem ? null
, inputs
, haskell-nix
, enableHaskellProfiling ? false
, source-repo-override ? { }
, evalSystem ? system
}:
let
  ownOverlays =
    [
      # Modifications to derivations from nixpkgs
      (import ./overlays/nixpkgs-overrides.nix)
      # fix r-modules
      (import ./overlays/r.nix)
      (import ./overlays/networks.nix)
      # stdenv.lib is still needed by the pinned version of easy purescipt
      (final: prev: { stdenv = prev.stdenv // { inherit (final) lib; }; })
    ];

  iohkNixMain = import inputs.iohk-nix { };

  nixpkgsArgs = {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      [ haskell-nix.overlay ]
      # our own overlays:
      # needed for cardano-api wich uses a patched libsodium
      ++ iohkNixMain.overlays.crypto
      ++ iohkNixMain.overlays.iohkNix
      ++ [
        (final: prev: {
          cardano = let c = inputs.cardano-world.${system}.cardano; in
            c // {
              entrypoints = c.entrypoints.override {
                inherit evalSystem;
              };
            };
        })
      ]
      ++ ownOverlays;
    inherit (haskell-nix) config;
    inherit crossSystem;
    # In nixpkgs versions older than 21.05, if we don't explicitly pass
    # in localSystem we will hit a code path that uses builtins.currentSystem,
    # which breaks flake's pure evaluation.
    localSystem = { inherit system; };
  };

  pkgs = import inputs.nixpkgs nixpkgsArgs;

  marlowe = import ./pkgs { inherit pkgs enableHaskellProfiling inputs source-repo-override system evalSystem; };

in
{
  inherit pkgs marlowe;
}
