{ pkgs, marlowe }:
pkgs.recurseIntoAttrs {
  site = pkgs.callPackage ../doc {
    inherit (marlowe) sphinx-markdown-tables sphinxemoji;
    inherit (marlowe.sphinxcontrib-haddock) sphinxcontrib-haddock sphinxcontrib-domaintools;
    combined-haddock = marlowe.plutus-haddock-combined;
    pythonPackages = pkgs.python3Packages;
  };

  build-and-serve-docs = pkgs.writeShellScriptBin "build-and-serve-docs" ''
    cd $(nix build .#docs.site --no-link --json | jq -r .[0].outputs.out) && \
    ${pkgs.python3}/bin/python3 -m http.server 8002
  '';
}
