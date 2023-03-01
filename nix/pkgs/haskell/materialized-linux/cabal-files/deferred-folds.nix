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
      identifier = { name = "deferred-folds"; version = "0.9.18.2"; };
      license = "MIT";
      copyright = "(c) 2018, Metrix.AI";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/metrix-ai/deferred-folds";
      url = "";
      synopsis = "Abstractions over deferred folds";
      description = "This library is in an experimental state.\nUsers should be prepared for frequent updates.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."deferred-folds" or (errorHandler.buildDepError "deferred-folds"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/deferred-folds-0.9.18.2.tar.gz";
      sha256 = "26ddc60183478033735f82ba506db93032fb8c7fe91b7e72a0d8f9f05febb42a";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: deferred-folds\nversion: 0.9.18.2\ncategory: Folding\nsynopsis: Abstractions over deferred folds\ndescription:\n  This library is in an experimental state.\n  Users should be prepared for frequent updates.\nstability: Experimental\nhomepage: https://github.com/metrix-ai/deferred-folds\nbug-reports: https://github.com/metrix-ai/deferred-folds/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2018, Metrix.AI\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\n\nsource-repository head\n  type: git\n  location: git://github.com/metrix-ai/deferred-folds.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    DeferredFolds.Unfoldl\n    DeferredFolds.UnfoldlM\n    DeferredFolds.Unfoldr\n  other-modules:\n    DeferredFolds.Types\n    DeferredFolds.UnfoldrM\n    DeferredFolds.Defs.Unfoldl\n    DeferredFolds.Defs.UnfoldlM\n    DeferredFolds.Defs.Unfoldr\n    DeferredFolds.Defs.UnfoldrM\n    DeferredFolds.Prelude\n    DeferredFolds.Util.TextArray\n  build-depends:\n    base >=4.9 && <5,\n    bytestring >=0.10 && <0.12,\n    containers >=0.5 && <0.7,\n    foldl >=1 && <2,\n    hashable >=1 && <2,\n    primitive >=0.6.4 && <0.8,\n    text >=1.2 && <1.3 || >=2.0 && <2.1,\n    transformers >=0.5 && <0.7,\n    unordered-containers >=0.2 && <0.3,\n    vector >=0.12 && <0.14,\n\ntest-suite test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, PatternSynonyms, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  main-is:\n    Main.hs\n  build-depends:\n    deferred-folds,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase <2,\n    tasty >=0.12 && <2,\n    tasty-hunit >=0.9 && <0.11,\n    tasty-quickcheck >=0.9 && <0.11,\n";
    }