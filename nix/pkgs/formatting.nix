{ writeShellScriptBinInRepoRoot
, pkgs
, easyPS
}:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs.nodePackages) prettier;
  inherit (easyPS) purs-tidy dhall-simple;
  extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";
  writeFormatter = name: cmd: extensions: writeShellScriptBinInRepoRoot "fix-${name}" ''
    set -e
    echo formatting with ${name}
    ${pkgs.git}/bin/git ls-files ':!:bitte/node/config/*'\
      | grep -E '${extensionsToRegex extensions}' \
      | xargs -d $'\\n' ${cmd}
    echo done.
  '';
  dhall-batch = pkgs.writeShellScriptBin "dhall" ''
    for f in "$@"; do ${dhall-simple}/bin/dhall format --inplace $f; done
  '';
in
{
  fix-prettier = writeFormatter
    "prettier"
    "${prettier}/bin/prettier -w"
    [ "js" "ts" "css" "html" ];
  fix-purs-tidy = writeFormatter
    "purs-tidy"
    "${purs-tidy}/bin/purs-tidy format-in-place"
    [ "purs" ];
  fix-dhall = writeFormatter "dhall" "${dhall-batch}/bin/dhall" [ "dhall" ];
  purs-tidy-hook = {
    enable = true;
    name = "purs-tidy";
    entry = "${purs-tidy}/bin/purs-tidy format-in-place";
    files = "\\.purs$";
    language = "system";
  };
  dhall-hook = {
    enable = true;
    name = "dhall";
    entry = "${dhall-batch}/bin/dhall";
    files = "\\.dhall$";
    language = "system";
  };
}
