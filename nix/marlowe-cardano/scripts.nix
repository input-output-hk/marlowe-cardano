{ nix, inputs', pkgs, ... }:

let

  compose-spec = nix.marlowe-cardano.compose;

  mkCabalExeScript = target: ''
    cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
    cabal build ${target} 1>/dev/null 2>/dev/null
    cabal run ${target} -- "$@" | tail -n +2
  '';
in
{
  re-up = ''
    cd $(git rev-parse --show-toplevel)

    docker compose -f ${compose-spec} up -d
  '';

  refresh-compose = ''
    cd $(git rev-parse --show-toplevel)
    nix-store --realise ${compose-spec} --add-root compose.yaml --indirect
  '';

  refresh-validators = ''
    cd $(git rev-parse --show-toplevel)
    mkdir -p marlowe/scripts
    cp ${inputs'.marlowe-plutus.packages.validators}/* marlowe/scripts
    chmod u+w marlowe/scripts/*
  '';

  marlowe-runtime-cli = mkCabalExeScript "marlowe-runtime-cli";

  marlowe-cli = mkCabalExeScript "marlowe-cli";
}
