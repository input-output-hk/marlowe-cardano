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
      identifier = { name = "bytestring-strict-builder"; version = "0.4.5.6"; };
      license = "MIT";
      copyright = "(c) 2017, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/bytestring-strict-builder";
      url = "";
      synopsis = "An efficient strict bytestring builder";
      description = "According to\n<https://github.com/nikita-volkov/bytestring-builders-benchmark the competition benchmarks>,\nthis library provides on average the fastest builder of strict bytestrings.\n\nPractical benchmarks have proven it to be highly performant as well.\nThe encoders from the \\\"postgresql-binary\\\" library have shown\na stable performance improvement by factors of up to 10 after the migration\nfrom the standard builder to \\\"bytestring-strict-builder\\\".";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."bytestring-strict-builder" or (errorHandler.buildDepError "bytestring-strict-builder"))
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
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."bytestring-strict-builder" or (errorHandler.buildDepError "bytestring-strict-builder"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bytestring-strict-builder-0.4.5.6.tar.gz";
      sha256 = "7c9261ff2d10eb141fead8eb57f51fec14b28e753084c7a07fc319776531117f";
      });
    }) // {
    package-description-override = "name: bytestring-strict-builder\nversion: 0.4.5.6\ncategory: Text, ByteString, Builders, Serialization\nsynopsis: An efficient strict bytestring builder\ndescription:\n  According to \n  <https://github.com/nikita-volkov/bytestring-builders-benchmark the competition benchmarks>, \n  this library provides on average the fastest builder of strict bytestrings. \n  .\n  Practical benchmarks have proven it to be highly performant as well.\n  The encoders from the \\\"postgresql-binary\\\" library have shown\n  a stable performance improvement by factors of up to 10 after the migration\n  from the standard builder to \\\"bytestring-strict-builder\\\".\nhomepage: https://github.com/nikita-volkov/bytestring-strict-builder\nbug-reports: https://github.com/nikita-volkov/bytestring-strict-builder/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2017, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/bytestring-strict-builder.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    ByteString.StrictBuilder\n  other-modules:\n    ByteString.StrictBuilder.Prelude\n    ByteString.StrictBuilder.Population\n    ByteString.StrictBuilder.Population.UncheckedShifting\n    ByteString.StrictBuilder.UTF8\n  build-depends:\n    base >=4.11 && <5,\n    bytestring >=0.10.2 && <0.12\n\ntest-suite tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Main.hs\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  build-depends:\n    bytestring-strict-builder,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase >=1.10 && <2,\n    tasty >=1.4 && <2,\n    tasty-hunit >=0.10 && <0.11,\n    tasty-quickcheck >=0.10 && <0.11\n\nbenchmark benchmarks\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmarks\n  main-is: Main.hs\n  ghc-options: -O2 -threaded \"-with-rtsopts=-N\" -funbox-strict-fields\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  build-depends:\n    bytestring-strict-builder,\n    criterion >=1.5.9 && <1.6,\n    rerebase >=1.10 && <2\n";
    }