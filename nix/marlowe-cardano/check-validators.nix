{ inputs, pkgs, ... }:

pkgs.runCommand "check-validators" { } ''
  mkdir -p $out/marlowe-plutus
  mkdir -p $out/local
  cd $out
  cp ${inputs.marlowe-plutus.packages.validators}/* marlowe-plutus
  cp ${../../marlowe/scripts}/* local
  ${pkgs.diffutils}/bin/diff local marlowe-plutus
''
