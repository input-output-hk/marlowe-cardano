{ nix, inputs', pkgs, ... }:

let

  re-up = ''
    cd $(git rev-parse --show-toplevel)

    docker compose -f ${compose-spec} up -d
  '';

  compose-spec = nix.marlowe-cardano.compose;

  refresh-compose = ''
    cd $(git rev-parse --show-toplevel)
    nix-store --realise ${compose-spec} --add-root compose.yaml --indirect
  '';

  mkCabalExeScript = target: ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    cabal build ${target} 1>/dev/null 2>/dev/null
    cabal run ${target} -- "$@" | tail -n +2
  '';

  marlowe-runtime-cli = mkCabalExeScript "marlowe-runtime-cli";

  marlowe-cli = mkCabalExeScript "marlowe-cli";
in
{
  inherit
    re-up
    refresh-compose
    marlowe-runtime-cli
    marlowe-cli;
}
