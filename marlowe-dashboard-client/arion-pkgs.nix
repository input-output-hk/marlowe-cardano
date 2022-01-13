let
  packages = import ../default.nix { };
in
packages.pkgs // packages
