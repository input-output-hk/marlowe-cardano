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
      identifier = { name = "text-builder-dev"; version = "0.3.3"; };
      license = "MIT";
      copyright = "(c) 2022, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/text-builder-dev";
      url = "";
      synopsis = "Edge of developments for \"text-builder\"";
      description = "This is a development version of \\\"text-builder\\\".\nAll experimentation and feature development happens here.\nThe API can change drastically.\nFor a more stable API use \\\"text-builder\\\",\nwhich is now just a wrapper over this package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deferred-folds" or (errorHandler.buildDepError "deferred-folds"))
          (hsPkgs."isomorphism-class" or (errorHandler.buildDepError "isomorphism-class"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
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
            (hsPkgs."text-builder-dev" or (errorHandler.buildDepError "text-builder-dev"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark-text" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."text-builder-dev" or (errorHandler.buildDepError "text-builder-dev"))
            ];
          buildable = true;
          };
        "benchmark-char" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."text-builder-dev" or (errorHandler.buildDepError "text-builder-dev"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-builder-dev-0.3.3.tar.gz";
      sha256 = "e468dd5b4439dbd369af68a7dd0448b1b6027e9edd76df7fbdcc1f0e64f70841";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: text-builder-dev\nversion: 0.3.3\ncategory: Text\nsynopsis: Edge of developments for \"text-builder\"\ndescription:\n  This is a development version of \\\"text-builder\\\".\n  All experimentation and feature development happens here.\n  The API can change drastically.\n  For a more stable API use \\\"text-builder\\\",\n  which is now just a wrapper over this package.\nhomepage: https://github.com/nikita-volkov/text-builder-dev\nbug-reports: https://github.com/nikita-volkov/text-builder-dev/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2022, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/text-builder-dev.git\n\ncommon language-settings\n  default-extensions: BangPatterns, ConstraintKinds, CPP, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeApplications, TypeFamilies, TypeOperators, ViewPatterns\n  default-language: Haskell2010\n\nlibrary\n  import: language-settings\n  hs-source-dirs: library\n  exposed-modules:\n    TextBuilderDev\n  other-modules:\n    TextBuilderDev.Unicode\n    TextBuilderDev.UTF16\n    TextBuilderDev.UTF8\n    TextBuilderDev.Prelude\n  build-depends:\n    base >=4.11 && <5,\n    bytestring >=0.10 && <0.12,\n    deferred-folds >=0.9.10.1 && <0.10,\n    isomorphism-class >=0.1.0.1 && <0.2,\n    split >=0.2.3.4 && <0.3,\n    text >=1.0 && <3,\n    transformers >=0.5 && <0.7,\n\ntest-suite test\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    TextBuilderDev.TastyExtras\n  build-depends:\n    QuickCheck >=2.13 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10.0.2 && <0.11,\n    tasty-quickcheck >=0.10.1 && <0.11,\n    text-builder-dev,\n\nbenchmark benchmark-text\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmark-text\n  main-is: Main.hs\n  build-depends:\n    criterion >=1.5.13.0 && <2,\n    rerebase ==1.*,\n    text-builder-dev,\n\nbenchmark benchmark-char\n  import: language-settings\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmark-char\n  main-is: Main.hs\n  build-depends:\n    criterion >=1.5.13.0 && <2,\n    rerebase ==1.*,\n    text-builder-dev,\n";
    }