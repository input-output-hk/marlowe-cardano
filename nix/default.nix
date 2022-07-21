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

  extraOverlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # our own overlays:
    # needed for cardano-api wich uses a patched libsodium
    ++ iohkNixMain.overlays.crypto
    ++ iohkNixMain.overlays.iohkNix
    ++ ownOverlays;


  # Belowe strategy to use old nixpkgs and custom overlay didn't work.
  # I've moved complately to nixpkgs from cardano-node flake.lock.
  #
  #    (final: prev: { secp256k1 = pkgsCardanoNode.secp256k1; })
  #
  # pkgsCardanoNode = import sources.nixpkgs-cardano-node {
  #   inherit crossSystem;
  #   # In nixpkgs versions older than 21.05, if we don't explicitly pass
  #   # in localSystem we will hit a code path that uses builtins.currentSystem,
  #   # which breaks flake's pure evaluation.
  #   localSystem = { inherit system; };
  #   config = haskellNix.nixpkgsArgs.config // config;
  # };
  # pkgs = import sources.nixpkgs {


  pkgs = import sources.nixpkgs-cardano-node-unstable {
    inherit crossSystem;
    # In nixpkgs versions older than 21.05, if we don't explicitly pass
    # in localSystem we will hit a code path that uses builtins.currentSystem,
    # which breaks flake's pure evaluation.
    localSystem = { inherit system; };
    overlays = extraOverlays ++ overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

  marlowe = import ./pkgs { inherit pkgs checkMaterialization enableHaskellProfiling sources source-repo-override; };

in
{
  inherit pkgs marlowe sources;
}
