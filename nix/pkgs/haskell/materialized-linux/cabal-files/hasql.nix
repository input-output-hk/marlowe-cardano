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
      identifier = { name = "hasql"; version = "1.5.0.5"; };
      license = "MIT";
      copyright = "(c) 2014, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/hasql";
      url = "";
      synopsis = "An efficient PostgreSQL driver with a flexible mapping API";
      description = "Root of the \\\"hasql\\\" ecosystem.\nFor details and tutorials see\n<https://github.com/nikita-volkov/hasql the readme>.\n\nThe API comes free from all kinds of exceptions. All error-reporting is explicit and is presented using the 'Either' type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-strict-builder" or (errorHandler.buildDepError "bytestring-strict-builder"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hashtables" or (errorHandler.buildDepError "hashtables"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."postgresql-binary" or (errorHandler.buildDepError "postgresql-binary"))
          (hsPkgs."postgresql-libpq" or (errorHandler.buildDepError "postgresql-libpq"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "tasty" = {
          depends = [
            (hsPkgs."contravariant-extras" or (errorHandler.buildDepError "contravariant-extras"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "threads-test" = {
          depends = [
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        "profiling" = {
          depends = [
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmarks" = {
          depends = [
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hasql-1.5.0.5.tar.gz";
      sha256 = "1c83abd2604c2182e5c51f00065222e6e52a8b9c5ef4f362c8061b9350d62b09";
      });
    }) // {
    package-description-override = "name: hasql\nversion: 1.5.0.5\ncategory: Hasql, Database, PostgreSQL\nsynopsis: An efficient PostgreSQL driver with a flexible mapping API\ndescription:\n  Root of the \\\"hasql\\\" ecosystem.\n  For details and tutorials see\n  <https://github.com/nikita-volkov/hasql the readme>.\n  .\n  The API comes free from all kinds of exceptions. All error-reporting is explicit and is presented using the 'Either' type.\nhomepage: https://github.com/nikita-volkov/hasql\nbug-reports: https://github.com/nikita-volkov/hasql/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2014, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/hasql.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    Hasql.Decoders\n    Hasql.Encoders\n    Hasql.Connection\n    Hasql.Statement\n    Hasql.Session\n  other-modules:\n    Hasql.Private.Prelude\n    Hasql.Private.Errors\n    Hasql.Private.PTI\n    Hasql.Private.IO\n    Hasql.Private.Session\n    Hasql.Private.Connection\n    Hasql.Private.PreparedStatementRegistry\n    Hasql.Private.Settings\n    Hasql.Private.Commands\n    Hasql.Private.Decoders\n    Hasql.Private.Decoders.Array\n    Hasql.Private.Decoders.Composite\n    Hasql.Private.Decoders.Value\n    Hasql.Private.Decoders.Row\n    Hasql.Private.Decoders.Result\n    Hasql.Private.Decoders.Results\n    Hasql.Private.Encoders\n    Hasql.Private.Encoders.Array\n    Hasql.Private.Encoders.Value\n    Hasql.Private.Encoders.Params\n  build-depends:\n    attoparsec >=0.10 && <0.15,\n    base >=4.12 && <5,\n    bytestring >=0.10 && <0.12,\n    bytestring-strict-builder >=0.4.5.1 && <0.5,\n    contravariant >=1.3 && <2,\n    dlist ==0.8.* || >=1 && <2,\n    hashable >=1.2 && <2,\n    hashtables >=1.1 && <2,\n    mtl >=2 && <3,\n    postgresql-binary >=0.12.4 && <0.13,\n    postgresql-libpq ==0.9.*,\n    profunctors >=5.1 && <6,\n    text >=1 && <3,\n    text-builder >=0.6.1.2 && <0.7,\n    transformers >=0.3 && <0.7,\n    vector >=0.10 && <0.14\n\ntest-suite tasty\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tasty\n  main-is: Main.hs\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  other-modules:\n    Main.DSL\n    Main.Connection\n    Main.Statements\n    Main.Prelude\n  build-depends:\n    contravariant-extras >=0.3.5.2 && <0.4,\n    hasql,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase <2,\n    tasty >=0.12 && <2,\n    tasty-hunit >=0.9 && <0.11,\n    tasty-quickcheck >=0.9 && <0.11\n\ntest-suite threads-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: threads-test\n  main-is: Main.hs\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  other-modules:\n    Main.Statements\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n  build-depends:\n    hasql,\n    rerebase\n\nbenchmark benchmarks\n  type: exitcode-stdio-1.0\n  hs-source-dirs: benchmarks\n  main-is: Main.hs\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n    -rtsopts\n    -funbox-strict-fields\n  build-depends:\n    gauge >=0.2.5 && <0.3,\n    hasql,\n    rerebase <2\n\ntest-suite profiling\n  type: exitcode-stdio-1.0\n  hs-source-dirs: profiling\n  main-is: Main.hs\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  ghc-options:\n    -O2\n    -threaded\n    -rtsopts\n  build-depends:\n    hasql,\n    rerebase ==1.*\n";
    }