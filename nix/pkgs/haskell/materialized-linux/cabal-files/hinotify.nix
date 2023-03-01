{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "hinotify"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Lennart Kolmodin <kolmodin@gmail.com>";
      author = "Lennart Kolmodin";
      homepage = "https://github.com/kolmodin/hinotify";
      url = "";
      synopsis = "Haskell binding to inotify";
      description = "This library provides a wrapper to the Linux Kernel's inotify feature,\nallowing applications to subscribe to notifications when a file is\naccessed or modified.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          ];
        libs = (pkgs.lib).optional (system.isFreebsd || system.isOpenbsd) (pkgs."inotify" or (errorHandler.sysDepError "inotify"));
        buildable = true;
        };
      tests = {
        "test001" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        "test002" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        "test003" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        "test004" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        "test005" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        "test006" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hinotify-0.4.1.tar.gz";
      sha256 = "1307b100aeaf35d0d0f582d4897fac9cde39505ec52c915e213118e56674f81a";
      });
    }) // {
    package-description-override = "name:               hinotify\nversion:            0.4.1\nbuild-type:         Simple\nsynopsis:           Haskell binding to inotify\ndescription:\n    This library provides a wrapper to the Linux Kernel's inotify feature,\n    allowing applications to subscribe to notifications when a file is\n    accessed or modified.\ncategory:           System\nhomepage:           https://github.com/kolmodin/hinotify\nbug-reports:        https://github.com/kolmodin/hinotify/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Lennart Kolmodin\nmaintainer:         Lennart Kolmodin <kolmodin@gmail.com>\nextra-source-files: README.md, CHANGELOG.md\ncabal-version:      >= 1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/kolmodin/hinotify.git\n\nlibrary\n    default-language: Haskell2010\n    build-depends:  base >= 4.5.0.0 && < 5, bytestring, containers, unix,\n                    async == 2.*\n\n    exposed-modules:\n        System.INotify\n    other-modules:\n        System.INotify.Masks\n\n    ghc-options: -Wall\n    includes: sys/inotify.h\n    hs-source-dirs: src\n\n    if os(freebsd) || os(openbsd)\n      extra-libraries: inotify\n\ntest-suite test001\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base, bytestring, directory, hinotify, unix\n    hs-source-dirs: src tests\n    main-is: test001-list-dir-contents.hs\n    other-modules: Utils\n    ghc-options: -Wall\n\ntest-suite test002\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base, bytestring, directory, hinotify, unix\n    hs-source-dirs: src tests\n    main-is: test002-writefile.hs\n    other-modules: Utils\n    ghc-options: -Wall\n\ntest-suite test003\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base, bytestring, directory, hinotify, unix\n    hs-source-dirs: src tests\n    main-is: test003-removefile.hs\n    other-modules: Utils\n    ghc-options: -Wall\n\ntest-suite test004\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    build-depends: base, bytestring, directory, hinotify, unix\n    hs-source-dirs: src tests\n    main-is: test004-modify-file.hs\n    other-modules: Utils\n    ghc-options: -Wall\n\ntest-suite test005\n    type: exitcode-stdio-1.0\n    build-depends: base, bytestring, directory, hinotify, unix\n    default-language: Haskell2010\n    hs-source-dirs: src tests\n    main-is: test005-move-file.hs\n    other-modules: Utils\n    ghc-options: -Wall\n\ntest-suite test006\n    type: exitcode-stdio-1.0\n    build-depends: base, bytestring, directory, hinotify, unix\n    default-language: Haskell2010\n    hs-source-dirs: src tests\n    main-is: test006-callbackHang.hs\n    other-modules: Utils\n    ghc-options: -Wall\n";
    }