# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#32-nixiogx-confignix

{
  systems = [ "x86_64-darwin" "x86_64-linux" ];
  haskellCompilers = [ "ghc8107" ];
  shouldCrossCompile = false;
}
