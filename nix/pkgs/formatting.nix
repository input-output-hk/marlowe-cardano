{ writeShellScriptBinInRepoRoot
, pkgs
}:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs.nodePackages) prettier;
  extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";
  writeFormatter = name: cmd: extensions: writeShellScriptBinInRepoRoot "fix-${name}" ''
    set -e
    echo formatting with ${name}
    ${pkgs.git}/bin/git ls-files ':!:bitte/node/config/*'\
      | grep -E '${extensionsToRegex extensions}' \
      | xargs -d $'\\n' ${cmd}
    echo done.
  '';
in
{
  fix-prettier = writeFormatter
    "prettier"
    "${prettier}/bin/prettier -w"
    [ "js" "ts" "css" "html" ];
}
