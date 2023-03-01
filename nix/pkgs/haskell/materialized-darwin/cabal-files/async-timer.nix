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
      identifier = { name = "async-timer"; version = "0.1.4.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2018 Moritz Clasmeier";
      maintainer = "mtesseract@silverratio.net";
      author = "Moritz Clasmeier";
      homepage = "https://github.com/mtesseract/async-timer#readme";
      url = "";
      synopsis = "Provides API for timer based execution of IO actions";
      description = "This is a lightweight package built on top of the async package\nproviding easy to use periodic timers. This can be used for executing\nIO actions periodically.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ];
        buildable = true;
        };
      tests = {
        "async-timer-test" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."async-timer" or (errorHandler.buildDepError "async-timer"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/async-timer-0.1.4.1.tar.gz";
      sha256 = "c2aa4d89792e6f21367122f4d9f1d5cafdf1e04a5a82fb566ec508453a83a398";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.20.0.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 591c3b4ee57f39cdee5778c4cedee60c71f1a62a5f5a1143919d71f77f00e0ac\n\nname:           async-timer\nversion:        0.1.4.1\nsynopsis:       Provides API for timer based execution of IO actions\ndescription:    This is a lightweight package built on top of the async package\n                providing easy to use periodic timers. This can be used for executing\n                IO actions periodically.\ncategory:       Concurrency\nhomepage:       https://github.com/mtesseract/async-timer#readme\nbug-reports:    https://github.com/mtesseract/async-timer/issues\nauthor:         Moritz Clasmeier\nmaintainer:     mtesseract@silverratio.net\ncopyright:      (c) 2016-2018 Moritz Clasmeier\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\nextra-source-files:\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/mtesseract/async-timer\n\nlibrary\n  hs-source-dirs:\n      src\n  ghc-options: -Wall -fno-warn-type-defaults\n  build-depends:\n      base >=4.9.1.0 && <5\n    , lifted-async >=0.9.1.1 && <0.11\n    , lifted-base >=0.2.3.11 && <0.3\n    , monad-control >=1.0.1.0 && <1.1\n    , safe-exceptions >=0.1.5.0 && <0.2\n    , transformers-base >=0.4.4 && <0.5\n  exposed-modules:\n      Control.Concurrent.Async.Timer\n      Control.Concurrent.Async.Timer.Unsafe\n  other-modules:\n      Control.Concurrent.Async.Timer.Internal\n      Paths_async_timer\n  default-language: Haskell2010\n\ntest-suite async-timer-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  hs-source-dirs:\n      test\n  default-extensions: OverloadedStrings\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      HUnit\n    , async-timer\n    , base\n    , containers\n    , criterion\n    , lifted-async\n    , test-framework\n    , test-framework-hunit\n  other-modules:\n      Paths_async_timer\n  default-language: Haskell2010\n";
    }