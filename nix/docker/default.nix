{ inputs, pkgs, lib, ... }:
let
  inherit (inputs) std n2c;

  mkOciImages =
    { defaultImageAttrs ? { }
    , imageSet
    }:
    let
      builtImageSet = lib.mapAttrs
        (_: imageConfig: mkImage (lib.recursiveUpdate imageConfig defaultImageAttrs))
        imageSet;

      forAllImages = f: lib.concatMapStrings (s: s + "\n") (lib.mapAttrsToList f builtImageSet);
    in
    builtImageSet // {
      all = {
        copyToDockerDaemon = std.lib.ops.writeScript {
          name = "copy-to-docker-daemon";
          text = forAllImages (name: img:
            "${n2c.packages.skopeo-nix2container}/bin/skopeo --insecure-policy copy nix:${img} docker-daemon:${name}:latest"
          );
        };
      };
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
in
{
  inherit mkOciImages;
}
