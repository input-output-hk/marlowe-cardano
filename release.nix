# TODO re-enable Darwin after Hydra Darwin build issues are resolved.
{ supportedSystems ? [ "x86_64-linux" /* "x86_64-darwin" */ ]
, rootsOnly ? false
  # We explicitly pass true here in the GitHub action but don't want to slow down hydra
, checkMaterialization ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { system = builtins.currentSystem; } // sourcesOverride
, plutus-apps ? null
}:
let
  inherit (import ./default.nix { }) pkgs;
  # If hydra passed us a plutus-apps checkout, we want to override the
  # plutus-apps used by haskell.nix
  source-repo-override = if plutus-apps == null then { } else {
    # Overwrite the source-repository-package entry with this URL
    "https://github.com/input-output-hk/plutus-apps.git" = orig: {
      url = plutus-apps.uri;
      ref = plutus-apps.rev;
      # Nix needs the sha256 of the checked-out source, but hydra
      # gives us the path itself, not its hash. This uses the
      # exportReferencesGraph feature of Nix to introspect properties
      # of a given store path (in this case, its sha256), creates
      # a Nix expression containing that hash, and imports that Nix
      # expression into the current evaluation to get the hash
      sha256 = import (pkgs.stdenv.mkDerivation {
        name = "plutus-apps-sha.nix";
        exportReferencesGraph.plutus-apps = plutus-apps;
        __structuredAttrs = true;
        PATH = pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ];
        builder = builtins.toFile "builder" ''
          . .attrs.sh
          jq '."plutus-apps"[0].narHash' < .attrs.json > "$(jq -r .outputs.out < .attrs.json)"
        '';
      });
      # We assume the new version has the same subpackages we did
      # originally.
      inherit (orig) subdirs;
    };
  };

  traceNames = prefix: builtins.mapAttrs (n: v:
    if builtins.isAttrs v
    then if v ? type && v.type == "derivation"
    then __trace ("found job " + prefix + n) v
    else __trace ("looking in " + prefix + n) traceNames (prefix + n + ".") v
    else v);
  inherit (import (sources.plutus-core + "/nix/lib/ci.nix")) stripAttrsForHydra filterDerivations derivationAggregate;

  ci = import ./ci.nix { inherit supportedSystems rootsOnly checkMaterialization sourcesOverride sources source-repo-override; };
  # ci.nix is a set of attributes that work fine as jobs (albeit in a slightly different structure, the platform comes
  # first), but we mainly just need to get rid of some extra attributes.
  ciJobsets = stripAttrsForHydra (filterDerivations ci);
in
traceNames "" (ciJobsets // { required = derivationAggregate "required-marlowe" ciJobsets; })
