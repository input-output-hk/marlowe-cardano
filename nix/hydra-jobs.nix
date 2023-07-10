# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#39-nixhydra-jobsnix
{ pkgs, ... }:
{
  includedPaths = pkgs.lib.optionalList pkgs.stdenv.hostPlatform.isLinux [
    "oci-images"
    "operables"
  ];
}
