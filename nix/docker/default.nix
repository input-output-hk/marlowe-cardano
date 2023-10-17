{ inputs, pkgs, lib, ... }:
let
  inherit (inputs) std n2c;

  mkOciImages =
    { defaultImageAttrs ? { }
    , selections ? { }
    , images
    }:
    assert lib.assertMsg (lib.isList images) "images must be a list of images.";
    let
      mkImageConfigSet =
        buildSetWith
          (imageConfig:
            # TODO: If the user sees this error they will have no clue where
            # it is comming from. We need to aid the user with attributing
            # this error to the code that causes it.
            # Two ideas
            # 1. Provide more information about the attribute set that is
            #    missing the "name" attribute.
            # 2. Provide a location where the attribute is defined
            #    (Probably more difficult).
            assert lib.assertMsg (imageConfig ? name) "Attribute \"name\" is missing.";
            imageConfig.name
          );

      allFunctions = [
        "copyToDockerDaemon"
        "copyToRegistry"
        "copyTo"
        "copyToPodman"
      ];

      mkHelpers = builtImageSet:
        {
          all = lib.genAttrs
            allFunctions
            (mkFuctionCallForImages (lib.attrValues builtImageSet));
        };

      mkSelections = imageConfigSet:
        mapAttrsValues
          ({ config ? { }, selector }: extenedWith mkHelpers (
            lib.mapAttrs
              (_: imageConfig:
                # NOTE: Selection config overrides all config
                mkImage (lib.recursiveUpdate imageConfig config)
              )
              (lib.filterAttrs selector imageConfigSet)
          ))
          selections;
    in
    let
      imageConfigSet = mapAttrsValues
        (lib.recursiveUpdate defaultImageAttrs)
        (mkImageConfigSet images);

      builtImageSet = lib.mapAttrs (_: mkImage) imageConfigSet;
    in
    builtImageSet // (mkHelpers builtImageSet) // (mkSelections imageConfigSet);

  mkImage =
    { name

      # Formally from mkOperable
    , package
    , runtimeScript
    , runtimeInputs ? [ ]
    , livenessProbe ? null
    , readinessProbe ? null

    , tag ? null
    , uid ? "65534"
    , gid ? "65534"
    , labels ? { }
    }: std.lib.ops.mkStandardOCI {
      inherit name tag uid gid labels;

      operable = std.lib.ops.mkOperable
        {
          inherit package runtimeInputs runtimeScript;
        }
      // lib.optionalAttrs (livenessProbe != null) {
        livenessProbe = std.lib.ops.writeScript livenessProbe;
      }
      // lib.optionalAttrs (readinessProbe != null) {
        readinessProbe = std.lib.ops.writeScript readinessProbe;
      };
    };

  /*
    Builds an attribute set from a list using a selector function to generate
    attribute keys.

    Asserts that no attribute keys conflict.
  */
  buildSetWith = selector: lib.foldl'
    (currentSet: val:
      let
        key = selector val;
      in
      # TODO: If the user sees this error they will have no clue where
        #     it is comming from. We need to aid the user with attributing
        #     this error to the code that causes it.
        #     Two ideas
        #     1. Provide more information about the attribute sets evaulated to
        #        the same key.
        #     2. Provide locations where of the attribute sets that evaulated to
        #        the same key. (Probably more difficult).
      assert lib.assertMsg (!(currentSet ? ${key})) "${key} is repeated.";
      currentSet // { ${key} = val; }
    )
    { };

  mkFuctionCallForImages = builtImages: functionName: pkgs.writeShellScriptBin
    "${functionName}-set"
    (lib.concatMapStringsSep
      "\n"
      (img: "${lib.getExe img.passthru.${functionName}} \"$@\"")
      builtImages
    );

  extenedWith = f: attrs: attrs // f attrs;

  mapAttrsValues = f: lib.mapAttrs (_: f);
in
{
  inherit mkOciImages;
}
