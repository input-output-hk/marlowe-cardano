{ inputs', pkgs, l, ... }:
let
  inherit (inputs') std n2c;

  mkImages = imageSet:
    let
      imageSet' = l.mapAttrs (_: mkImage) imageSet;
      forAllImages = f: l.concatMapStrings (s: s + "\n") (l.mapAttrsToList f imageSet');
    in
    imageSet' // {
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
      // l.optionalAttrs (livenessProbe != null) {
        livenessProbe = std.lib.ops.writeScript livenessProbe;
      }
      // l.optionalAttrs (readinessProbe != null) {
        readinessProbe = std.lib.ops.writeScript readinessProbe;
      };
    };
in
{
  inherit mkImage mkImages;
}
