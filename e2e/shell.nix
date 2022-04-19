let pkgs = import <nixpkgs> { }; in
pkgs.mkYarnPackage {
  src = ./.;
  shellHook = pkgs.yarn2nix-moretea.linkNodeModulesHook;
  PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = true;
  PLAYWRIGHT_BROWSERS_PATH = 0;
}
