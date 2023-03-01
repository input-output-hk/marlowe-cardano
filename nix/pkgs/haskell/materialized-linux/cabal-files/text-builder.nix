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
      specVersion = "3.0";
      identifier = { name = "text-builder"; version = "0.6.7"; };
      license = "MIT";
      copyright = "(c) 2017, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/text-builder";
      url = "";
      synopsis = "An efficient strict text builder";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-builder-dev" or (errorHandler.buildDepError "text-builder-dev"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark-text" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
            ];
          buildable = true;
          };
        "benchmark-char" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-builder-0.6.7.tar.gz";
      sha256 = "90704ab83910db16d43aeb45e1bab1409f995e38c8c15101602eb38b9724f402";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: text-builder\nversion: 0.6.7\ncategory: Text\nsynopsis: An efficient strict text builder\nhomepage: https://github.com/nikita-volkov/text-builder\nbug-reports: https://github.com/nikita-volkov/text-builder/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2017, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/text-builder.git\n\ncommon language-settings\n  default-extensions: BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n\nlibrary\n  import: language-settings\n  hs-source-dirs: library\n  exposed-modules:\n    Text.Builder\n  other-modules:\n    Text.Builder.Prelude\n  build-depends:\n    base >=4.11 && <5,\n    bytestring >=0.10 && <0.12,\n    text >=1.2 && <3,\n    text-builder-dev >=0.3.1 && <0.4,\n\ntest-suite test\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  build-depends:\n    QuickCheck >=2.13 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10.0.2 && <0.11,\n    tasty-quickcheck >=0.10.1 && <0.11,\n    text-builder,\n\nbenchmark benchmark-text\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmark-text\n  ghc-options: -O2 -threaded \"-with-rtsopts=-N\" -funbox-strict-fields\n  main-is: Main.hs\n  build-depends:\n    criterion >=1.5.6.1 && <2,\n    rerebase ==1.*,\n    text-builder,\n\nbenchmark benchmark-char\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmark-char\n  ghc-options: -O2 -threaded \"-with-rtsopts=-N\" -funbox-strict-fields\n  main-is: Main.hs\n  build-depends:\n    criterion >=1.5.6.1 && <2,\n    rerebase ==1.*,\n    text-builder,\n";
    }