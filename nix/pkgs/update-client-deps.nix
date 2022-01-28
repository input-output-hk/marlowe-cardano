{ stdenv
, lib
, writeShellScriptBinInRepoRoot
, git
, fd
, coreutils
, python
, gnumake
, gnused
, nodejs
, nodePackages
, purs
, spago
, spago2nix
, clang
}:

let
  doUpdate = dir: name: ''
    pushd ${dir}
    echo Generating nix configs for ${name}.
    # Sometimes this command fails on a fresh fetch. So we retry it if it does.
    spago2nix generate || spago2nix generate
    popd
  '';

in
lib.meta.addMetaAttrs { platforms = lib.platforms.linux; } (writeShellScriptBinInRepoRoot "update-client-deps" ''
  set -eou pipefail

  export PATH=${lib.makeBinPath ([
    coreutils
    git
    python
    gnumake
    gnused
    nodejs
    nodePackages.node-gyp
    purs
    spago
    spago2nix
  ] ++ lib.optionals stdenv.isDarwin [ clang ])}

  ${doUpdate "marlowe-dashboard-client" "Marlowe Run"}
  ${doUpdate "marlowe-playground-client" "Marlowe Play"}
  echo Done
'')
