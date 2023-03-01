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
      specVersion = "1.12";
      identifier = { name = "fsnotify"; version = "0.4.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Tom McLaughlin <tom@codedown.io>";
      author = "Mark Dittmer <mark.s.dittmer@gmail.com>, Niklas Broberg";
      homepage = "https://github.com/haskell-fswatch/hfsnotify";
      url = "";
      synopsis = "Cross platform library for file change notification.";
      description = "Cross platform library for file creation, modification, and deletion notification. This library builds upon existing libraries for platform-specific Windows, Mac, and Linux filesystem event notification.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          ] ++ (pkgs.lib).optionals (system.isLinux) [
          (hsPkgs."hinotify" or (errorHandler.buildDepError "hinotify"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          ]) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ (pkgs.lib).optional (system.isOsx) (hsPkgs."hfsevents" or (errorHandler.buildDepError "hfsevents"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ] ++ (if system.isWindows
            then [
              (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
              (hsPkgs."sandwich" or (errorHandler.buildDepError "sandwich"))
              ]
            else [
              (hsPkgs."sandwich" or (errorHandler.buildDepError "sandwich"))
              ]);
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fsnotify-0.4.1.0.tar.gz";
      sha256 = "d130fb4a871a181a3d1485f24da85aa49d6ff9022ce08f8401b4626143eaf7ef";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.35.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:           fsnotify\nversion:        0.4.1.0\nsynopsis:       Cross platform library for file change notification.\ndescription:    Cross platform library for file creation, modification, and deletion notification. This library builds upon existing libraries for platform-specific Windows, Mac, and Linux filesystem event notification.\ncategory:       Filesystem\nhomepage:       https://github.com/haskell-fswatch/hfsnotify\nauthor:         Mark Dittmer <mark.s.dittmer@gmail.com>, Niklas Broberg\nmaintainer:     Tom McLaughlin <tom@codedown.io>\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    CHANGELOG.md\n    test/Main.hs\n\nlibrary\n  exposed-modules:\n      System.FSNotify\n      System.FSNotify.Devel\n  other-modules:\n      System.FSNotify.Find\n      System.FSNotify.Listener\n      System.FSNotify.Path\n      System.FSNotify.Polling\n      System.FSNotify.Types\n  hs-source-dirs:\n      src\n  ghc-options: -Wall\n  build-depends:\n      async >=2.0.0.0\n    , base >=4.8 && <5\n    , bytestring >=0.10.2\n    , containers >=0.4\n    , directory >=1.3.0.0\n    , filepath >=1.3.0.0\n    , monad-control >=1.0.0.0\n    , safe-exceptions >=0.1.0.0\n    , text >=0.11.0\n    , time >=1.1\n    , unix-compat >=0.2\n  default-language: Haskell2010\n  if os(linux)\n    cpp-options: -DOS_Linux\n  if os(windows)\n    cpp-options: -DOS_Win32\n  if os(darwin)\n    cpp-options: -DOS_Mac\n  if os(freebsd) || os(netbsd) || os(openbsd)\n    cpp-options: -DOS_BSD\n  if os(linux)\n    other-modules:\n        System.FSNotify.Linux\n    build-depends:\n        hinotify >=0.3.7\n      , unix >=2.7.1.0\n  if os(windows)\n    other-modules:\n        System.FSNotify.Win32\n        System.Win32.FileNotify\n        System.Win32.Notify\n    hs-source-dirs:\n        win-src\n    build-depends:\n        Win32\n  if os(darwin)\n    other-modules:\n        System.FSNotify.OSX\n    build-depends:\n        hfsevents >=0.1.3\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  main-is: Main.hs\n  other-modules:\n      FSNotify.Test.EventTests\n      FSNotify.Test.Util\n      Paths_fsnotify\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -Wall\n  build-depends:\n      async >=2\n    , base >=4.3.1.0\n    , directory\n    , exceptions\n    , filepath\n    , fsnotify\n    , random\n    , retry\n    , safe-exceptions\n    , temporary\n    , unix-compat\n    , unliftio >=0.2.20\n  default-language: Haskell2010\n  if os(linux)\n    cpp-options: -DOS_Linux\n  if os(windows)\n    cpp-options: -DOS_Win32\n  if os(darwin)\n    cpp-options: -DOS_Mac\n  if os(freebsd) || os(netbsd) || os(openbsd)\n    cpp-options: -DOS_BSD\n  if os(windows)\n    build-depends:\n        Win32\n      , sandwich >=0.1.1.1\n  else\n    build-depends:\n        sandwich\n";
    }