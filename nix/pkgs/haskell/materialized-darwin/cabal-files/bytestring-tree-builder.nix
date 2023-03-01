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
      identifier = { name = "bytestring-tree-builder"; version = "0.2.7.10"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/bytestring-tree-builder";
      url = "";
      synopsis = "A very efficient ByteString builder implementation based on the binary tree";
      description = "According to\n<https://github.com/nikita-volkov/bytestring-builders-benchmark the benchmarks>\nthis builder implementation beats all the alternatives.\nIt is especially well-suited for generating strict bytestrings,\nbeating the standard builder by at least the factor of 4.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "tasty" = {
          depends = [
            (hsPkgs."base-prelude" or (errorHandler.buildDepError "base-prelude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-tree-builder" or (errorHandler.buildDepError "bytestring-tree-builder"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base-prelude" or (errorHandler.buildDepError "base-prelude"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bytestring-tree-builder" or (errorHandler.buildDepError "bytestring-tree-builder"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.10.tar.gz";
      sha256 = "29282a224cd60c5c15a21deca658eb764325512ee96be8bc27638764aebd363d";
      });
    }) // {
    package-description-override = "name: bytestring-tree-builder\nversion: 0.2.7.10\ncategory: ByteString\nsynopsis: A very efficient ByteString builder implementation based on the binary tree\ndescription:\n  According to\n  <https://github.com/nikita-volkov/bytestring-builders-benchmark the benchmarks>\n  this builder implementation beats all the alternatives.\n  It is especially well-suited for generating strict bytestrings,\n  beating the standard builder by at least the factor of 4.\nhomepage: https://github.com/nikita-volkov/bytestring-tree-builder\nbug-reports: https://github.com/nikita-volkov/bytestring-tree-builder/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2015, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/bytestring-tree-builder.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  other-modules:\n    ByteString.TreeBuilder.Prelude\n    ByteString.TreeBuilder.Poker\n    ByteString.TreeBuilder.Tree\n  exposed-modules:\n    ByteString.TreeBuilder\n  build-depends:\n    base >=4.11 && <5,\n    bytestring >=0.10 && <0.12,\n    text >=1 && <3\n\nbenchmark benchmark\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmark\n  main-is: Main.hs\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n    -funbox-strict-fields\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  build-depends:\n    base-prelude,\n    bytestring,\n    bytestring-tree-builder,\n    criterion >=1.1 && <2,\n    deepseq ==1.*\n\ntest-suite tasty\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tasty\n  main-is: Main.hs\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  build-depends:\n    base-prelude,\n    bytestring,\n    bytestring-tree-builder,\n    QuickCheck >=2.14 && <3,\n    quickcheck-instances >=0.3.25 && <0.4,\n    tasty >=1.4 && <2,\n    tasty-hunit >=0.10 && <0.11,\n    tasty-quickcheck >=0.10 && <0.11\n";
    }