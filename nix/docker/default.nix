{ inputs, pkgs, lib, ... }:
let
  inherit (inputs) std n2c;

  mkOciImages =
    { defaultImageAttrs ? { }
    , images
    }:
    let
      builtImageSet = lib.mapAttrs
        (_: imageConfig: mkImage (lib.recursiveUpdate imageConfig defaultImageAttrs))
        images;

      allFunctions = [
        "copyToDockerDaemon"
        "copyToRegistry"
        "copyTo"
        "copyToPodman"
      ];
    in
    builtImageSet // {
      all = lib.genAttrs
        allFunctions
        (mkFuctionCallForImages (lib.attrValues builtImageSet));
    };

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

  mkFuctionCallForImages = builtImages: functionName: pkgs.writeShellScriptBin
    "${functionName}-set"
    (lib.concatMapStringsSep
      "\n"
      (img: "${lib.getExe img.passthru.${functionName}} \"$@\"")
      builtImages
    );
in
{
  inherit mkOciImages;
}
