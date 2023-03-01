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
      specVersion = "1.8";
      identifier = { name = "stm-delay"; version = "0.1.1.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) Joseph Adams 2012";
      maintainer = "joeyadams3.14159@gmail.com";
      author = "Joey Adams";
      homepage = "https://github.com/joeyadams/haskell-stm-delay";
      url = "";
      synopsis = "Updatable one-shot timer polled with STM";
      description = "This library lets you create a one-shot timer, poll it using STM,\nand update it to ring at a different time than initially specified.\n\nIt uses GHC event manager timeouts when available\n(GHC 7.2+, @-threaded@, non-Windows OS), yielding performance similar\nto @threadDelay@ and @registerDelay@.  Otherwise, it falls back to\nforked threads and @threadDelay@.\n\n[0.1.1]\nAdd tryWaitDelayIO, improve performance for certain cases of @newDelay@\nand @updateDelay@, and improve example.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-delay" or (errorHandler.buildDepError "stm-delay"))
            ];
          buildable = true;
          };
        "test-threaded" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-delay" or (errorHandler.buildDepError "stm-delay"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/stm-delay-0.1.1.1.tar.gz";
      sha256 = "b132581aac47e6cba6a1691a485e1700fbb047c02b7e1e43ae9bbd8476108a32";
      });
    }) // {
    package-description-override = "name:               stm-delay\nversion:            0.1.1.1\nsynopsis:           Updatable one-shot timer polled with STM\ndescription:\n    This library lets you create a one-shot timer, poll it using STM,\n    and update it to ring at a different time than initially specified.\n    .\n    It uses GHC event manager timeouts when available\n    (GHC 7.2+, @-threaded@, non-Windows OS), yielding performance similar\n    to @threadDelay@ and @registerDelay@.  Otherwise, it falls back to\n    forked threads and @threadDelay@.\n    .\n    [0.1.1]\n        Add tryWaitDelayIO, improve performance for certain cases of @newDelay@\n        and @updateDelay@, and improve example.\nhomepage:           https://github.com/joeyadams/haskell-stm-delay\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Joey Adams\nmaintainer:         joeyadams3.14159@gmail.com\ncopyright:          Copyright (c) Joseph Adams 2012\ncategory:           System\nbuild-type:         Simple\ncabal-version:      >= 1.8\n\nsource-repository head\n    type:       git\n    location:   git://github.com/joeyadams/haskell-stm-delay.git\n\nlibrary\n    exposed-modules:\n        Control.Concurrent.STM.Delay\n\n    ghc-options: -Wall -fwarn-tabs\n\n    build-depends: base >= 4.3 && < 5\n                 , stm\n\n    -- Need base >= 4.3 for:\n    --\n    --  * Control.Exception.mask\n    --\n    --  * forkIOUnmasked\n    --\n    --  * A threadDelay that doesn't give (-1) magic treatment.\n    --    See http://hackage.haskell.org/trac/ghc/ticket/2892\n    --\n    --  * GHC.Event (called System.Event in base 4.3)\n\ntest-suite test\n    type: exitcode-stdio-1.0\n\n    hs-source-dirs: test\n    main-is: Main.hs\n\n    ghc-options: -Wall\n                 -fno-warn-missing-signatures\n                 -fno-warn-name-shadowing\n                 -fno-warn-unused-do-bind\n                 -fno-warn-unused-matches\n\n    build-depends: base >= 4.3 && < 5\n                 , stm\n                 , stm-delay\n\ntest-suite test-threaded\n    type: exitcode-stdio-1.0\n\n    hs-source-dirs: test\n    main-is: Main.hs\n\n    ghc-options: -Wall -threaded\n                 -fno-warn-missing-signatures\n                 -fno-warn-name-shadowing\n                 -fno-warn-unused-do-bind\n                 -fno-warn-unused-matches\n\n    build-depends: base >= 4.3 && < 5\n                 , stm\n                 , stm-delay\n";
    }