{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, overlays ? [ ]
, sourcesOverride ? { }
, sources
, haskellNix
, checkMaterialization ? false
, enableHaskellProfiling ? false
, source-repo-override
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

  iohkNixMain = import sources.iohk-nix { };

  nixpkgsArgs = haskellNix.nixpkgsArgs // {
    overlays =
      # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
      haskellNix.nixpkgsArgs.overlays
        # our own overlays:
        # needed for cardano-api wich uses a patched libsodium
        ++ iohkNixMain.overlays.crypto
        ++ iohkNixMain.overlays.iohkNix
        ++ [ (final: prev: { cardano = (import sources.cardano-world { nixpkgs = final; }).${system}.cardano; }) ]
        ++ ownOverlays
        ++ overlays;
    config = haskellNix.nixpkgsArgs.config // config;
    # In nixpkgs versions older than 21.05, if we don't explicitly pass
    # in localSystem we will hit a code path that uses builtins.currentSystem,
    # which breaks flake's pure evaluation.
    localSystem = { inherit system; };
  };

  pkgs = import haskellNix.sources.nixpkgs-unstable nixpkgsArgs;

  marlowe = import ./pkgs { inherit pkgs checkMaterialization enableHaskellProfiling sources source-repo-override; };

in
{
  inherit pkgs marlowe sources;
}
