{ inputs', pkgs, l, ... }:
let
  inherit (inputs') std n2c;

  mkImages =
    { mkPublishDestinationFn ? null
    }:
    images:
    let
      imageSet = l.mapAttrs
        (n: i:
          let
            i' = mkImage i;
          in
          l.recursiveUpdate i' (l.optionalAttrs (mkPublishDestinationFn != null) {
            passthru.publish = mkPublish i' (mkPublishDestinationFn n);
          })
        )
        (buildSetWith
          (attr:
            # TODO: If the user sees this error they will have no clue where
            # it is comming from. We need to aid the user with attributing
            # this error to the code that causes it.
            # Two ideas
            # 1. Provide more information about the attribute set that is
            #    missing the "name" attribute.
            # 2. Provide a location where the attribute is defined
            #    (Probably more difficult).
            assert l.assertMsg (attr ? name) "Attribute \"name\" is missing.";
            attr.name
          )
          images
        );

      allFunctions = [
        "copyToDockerDaemon"
        "copyToRegistry"
        "copyTo"
        "copyToPodman"
      ] ++ l.optional (mkPublishDestinationFn != null) "publish";

    in
    imageSet // {
      all = l.genAttrs
        allFunctions
        (mkFuctionCallForImages (l.attrValues imageSet));
    };

  mkImage =
    { name

      # Formally from mkOperable
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
          inherit runtimeInputs runtimeScript;
          # NOTE: Fake package to get around std artificial requirements
          package = { inherit name; };
        }
      // l.optionalAttrs (livenessProbe != null) {
        livenessProbe = std.lib.ops.writeScript livenessProbe;
      }
      // l.optionalAttrs (readinessProbe != null) {
        readinessProbe = std.lib.ops.writeScript readinessProbe;
      };
    };

  /*
    Builds an attribute set from a list using a selector function to generate
    attribute keys.

    Asserts that no attribute keys conflict.
  */
  buildSetWith = selector: l.foldl'
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
      assert l.assertMsg (!(currentSet ? ${key})) "${key} is repeated.";
      currentSet // { ${key} = val; }
    )
    { };

  mkFuctionCallForImages = images: functionName: pkgs.writeShellScriptBin
    "${functionName}-set"
    (l.concatMapStringsSep
      "\n"
      (img: "${l.getExe img.passthru.${functionName}} \"$@\"")
      images
    );

  mkPublish = image: destinationString: pkgs.writeShellScriptBin "publish"
    ''
      echo Publishing ${image} to ${destinationString}
      ${l.getExe image.passthru.copyTo} ${destinationString}
    '';
in
{
  inherit mkImage mkImages;
}
