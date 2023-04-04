{
  description = "JupyterLab Flake with Marlowe Environment, including ghci and hoogle";

  inputs = {
    jupyterWith.url = "github:tweag/jupyterWith";
    flake-utils.url = "github:numtide/flake-utils";
    here.url = "..";
  };

  outputs = { self, nixpkgs, jupyterWith, flake-utils, here }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          system = system;
          overlays = nixpkgs.lib.attrValues jupyterWith.overlays;
        };
        local = here.packages.${system};
        ghcWithHoogle = local.marlowe.haskell.project.ghcWithHoogle (p: [
          p.marlowe
          p.marlowe-actus
          p.marlowe-chain-sync
          p.marlowe-cli
          # p.marlowe-contract
          p.marlowe-contracts
          p.marlowe-protocols
          p.marlowe-runtime
          # p.marlowe-symbolic
        ]);
        ibash = pkgs.jupyterWith.kernels.bashKernel {
          name = "Marlowe";
        };
        jupyterEnvironment = pkgs.jupyterlabWith {
          kernels = [
            ibash
          ];
          extraPackages = p: [
            local.marlowe-cli
            local.marlowe-runtime-cli
            # local.pkgs.cardano.packages.cardano-address
            # local.pkgs.cardano.packages.cardano-node
            local.pkgs.cardano.packages.cardano-cli
            p.z3
            p.coreutils
            p.curl
            p.gnused
            p.jq
            p.remarshal
            ghcWithHoogle
          ];
        };
      in
      {
        apps = rec {
          default = jupyter;
          jupyter = {
            type = "app";
            program = "${jupyterEnvironment}/bin/jupyter-lab";
          };
          #   ghci = {
          #     type = "app";
          #     program = "${ghcWithHoogle}/bin/ghci";
          #   };
          #   hoogle = {
          #     type = "app";
          #     program = "${jupyterEnvironment}/bin/hoogle";
          #   # program = "${jupyterEnvironment}/bin/hoogle server --no-security-headers --local --port 8079";
          #   };
        };
        devShell = jupyterEnvironment.env;
        z = pkgs;
        # devShell = pkgs.mkShell {
        #   buildInputs = [
        #     jupyterEnvironment
        #   ];
        #   shellHook = ''
        #     export PATH="${jupyterEnvironment}/bin:$PATH"
        #     export PS1="\n\[\033[1;32m\][nix develop:\w]\$\[\033[0m\] "
        #   '';
        # };
      }
    );
}
