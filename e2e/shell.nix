let
  inherit (import ../. { }) pkgs;

  browsers = pkgs.runCommand "playwright-browsers" { } ''
    mkdir -p $out/chromium-${browser-versions.chromium}/chrome-linux
    ln -sv ${pkgs.chromium}/bin/chromium $out/chromium-${browser-versions.chromium}/chrome-linux/chrome

    mkdir -p $out/ffmpeg-${browser-versions.ffmpeg}
    ln -sv ${pkgs.ffmpeg}/bin/ffmpeg $out/ffmpeg-${browser-versions.ffmpeg}/ffmpeg-linux
  '';

  browser-versions = builtins.listToAttrs (map
    (browser: {
      name = browser.name;
      value = browser.revision;
    })
    ((builtins.fromJSON (builtins.unsafeDiscardStringContext ((builtins.readFile "${pkg.node_modules}/playwright-core/browsers.json")))).browsers));

  pkg = pkgs.mkYarnPackage
    {
      src = ./.;
      shellHook = pkgs.yarn2nix-moretea.linkNodeModulesHook;
    } // pkgs.lib.optionalAttrs (! pkgs.stdenv.isDarwin) {
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = true;
    PLAYWRIGHT_BROWSERS_PATH = browsers;
  };
in
pkg
