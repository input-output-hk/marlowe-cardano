let
  packages = (builtins.getFlake "git+file:${toString ../.}").packages.${builtins.currentSystem};
in
packages.pkgs // packages
