{
  description = "JupyterLab Flake with Marlowe Environment";

  inputs = {
    jupyterWith.url = "github:tweag/jupyterWith";
    flake-utils.url = "github:numtide/flake-utils";
    marlowe-source = {
      url = "../";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, jupyterWith, flake-utils, marlowe-source }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          system = system;
          overlays = nixpkgs.lib.attrValues jupyterWith.overlays;
        };
        marlowe = import marlowe-source.outPath { inherit system; };
        ghcWithMarlowe = marlowe.marlowe.haskell.project.ghcWithPackages (p: [
          p.marlowe
        ]);
        ibash = pkgs.jupyterWith.kernels.bashKernel {
          name = "Marlowe";
        };
        jupyterEnvironment = pkgs.jupyterlabWith {
          kernels = [
            ibash
          ];
          extraPackages = p: [
            marlowe.cardano-cli
            marlowe.marlowe-cli
            p.coreutils
            p.curl
            p.gnused
            p.jq
            p.remarshal
            ghcWithMarlowe
          ];
        };
      in
      rec {
        apps.jupterlab = {
          type = "app";
          program = "${jupyterEnvironment}/bin/jupyter-lab";
        };
        defaultApp = apps.jupterlab;
        devShell = jupyterEnvironment.env;
      }
    );
}
