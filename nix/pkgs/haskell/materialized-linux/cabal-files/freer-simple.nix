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
      specVersion = "2.4";
      identifier = { name = "freer-simple"; version = "1.2.1.2"; };
      license = "BSD-3-Clause";
      copyright = "2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King";
      maintainer = "Alexis King <lexi.lambda@gmail.com>";
      author = "Allele Dev, Ixcom Core Team, Alexis King, and other contributors";
      homepage = "https://github.com/lexi-lambda/freer-simple";
      url = "";
      synopsis = "A friendly effect system for Haskell.";
      description = "An implementation of an effect system for Haskell (a fork of\n<http://hackage.haskell.org/package/freer-effects freer-effects>), which is\nbased on the work of Oleg Kiselyov et al.:\n\n* <http://okmij.org/ftp/Haskell/extensible/more.pdf Freer Monads, More Extensible Effects>\n* <http://okmij.org/ftp/Haskell/zseq.pdf Reflection without Remorse>\n* <http://okmij.org/ftp/Haskell/extensible/exteff.pdf Extensible Effects>\n\nThe key features are:\n\n* An efficient effect system for Haskell - as a library!\n* Reimplementations of several common Haskell monad transformers as effects.\n* Core components for defining your own Effects.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      exes = {
        "freer-simple-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            ];
          buildable = true;
          };
        };
      tests = {
        "freer-simple-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "freer-simple-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
            (hsPkgs."free" or (errorHandler.buildDepError "free"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/freer-simple-1.2.1.2.tar.gz";
      sha256 = "9c02bb4930ef245525e6ff8b548f77814c0b69acdecc48e100acab3b9b73d787";
      });
    }) // {
    package-description-override = "cabal-version: 2.4\nname: freer-simple\nversion: 1.2.1.2\ncategory: Control\nbuild-type: Simple\n\nsynopsis: A friendly effect system for Haskell.\ndescription:\n  An implementation of an effect system for Haskell (a fork of\n  <http://hackage.haskell.org/package/freer-effects freer-effects>), which is\n  based on the work of Oleg Kiselyov et al.:\n  .\n    * <http://okmij.org/ftp/Haskell/extensible/more.pdf Freer Monads, More Extensible Effects>\n    * <http://okmij.org/ftp/Haskell/zseq.pdf Reflection without Remorse>\n    * <http://okmij.org/ftp/Haskell/extensible/exteff.pdf Extensible Effects>\n  .\n  The key features are:\n  .\n    * An efficient effect system for Haskell - as a library!\n    * Reimplementations of several common Haskell monad transformers as effects.\n    * Core components for defining your own Effects.\n\nauthor: Allele Dev, Ixcom Core Team, Alexis King, and other contributors\nmaintainer: Alexis King <lexi.lambda@gmail.com>\ncopyright: 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King\nlicense: BSD-3-Clause\nlicense-file: LICENSE\nhomepage: https://github.com/lexi-lambda/freer-simple\nbug-reports: https://github.com/lexi-lambda/freer-simple/issues\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/lexi-lambda/freer-simple\n\ncommon common\n  ghc-options:\n    -Wall\n    -Wcompat\n    -Wincomplete-record-updates\n    -Wincomplete-uni-patterns\n    -Wredundant-constraints\n\n  default-language: Haskell2010\n  default-extensions:\n    ConstraintKinds\n    DataKinds\n    DeriveFunctor\n    FlexibleContexts\n    FlexibleInstances\n    FunctionalDependencies\n    GADTs\n    LambdaCase\n    MultiParamTypeClasses\n    RankNTypes\n    ScopedTypeVariables\n    TypeApplications\n    TypeOperators\n\n  build-depends: base >= 4.9 && < 5\n\nlibrary\n  import: common\n  hs-source-dirs: src\n  exposed-modules:\n    Control.Monad.Freer\n    Control.Monad.Freer.Coroutine\n    Control.Monad.Freer.Error\n    Control.Monad.Freer.Fresh\n    Control.Monad.Freer.Internal\n    Control.Monad.Freer.NonDet\n    Control.Monad.Freer.Reader\n    Control.Monad.Freer.State\n    Control.Monad.Freer.TH\n    Control.Monad.Freer.Trace\n    Control.Monad.Freer.Writer\n    Data.FTCQueue\n    Data.OpenUnion\n    Data.OpenUnion.Internal\n\n  build-depends:\n    , natural-transformation >= 0.2\n    , transformers-base\n    , template-haskell >= 2.11 && < 2.19\n\nexecutable freer-simple-examples\n  import: common\n  hs-source-dirs: examples/src\n  main-is: Main.hs\n  other-modules:\n    Capitalize\n    Console\n    Coroutine\n    Fresh\n    Trace\n\n  build-depends: freer-simple\n\ntest-suite freer-simple-test\n  import: common\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Tests.hs\n  other-modules:\n    Tests.Coroutine\n    Tests.Exception\n    Tests.Fresh\n    Tests.Loop\n    Tests.NonDet\n    Tests.Reader\n    Tests.State\n    Tests.TH\n\n  build-depends:\n    , QuickCheck\n    , freer-simple\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n\nbenchmark freer-simple-bench\n  import: common\n  type: exitcode-stdio-1.0\n  hs-source-dirs: bench\n  main-is: Core.hs\n  ghc-options: -O2\n\n  build-depends:\n    , criterion\n    , extensible-effects\n    , free\n    , freer-simple\n    , mtl\n";
    }