# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#39-nixcinix

{ pkgs, l, ... }:
{
  includedPaths = l.optionals pkgs.stdenv.hostPlatform.isLinux [
    "oci-images"
    "operables"
    "static"
  ];
}
