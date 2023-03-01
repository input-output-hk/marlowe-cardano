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
      identifier = { name = "postgresql-syntax"; version = "0.4.1"; };
      license = "MIT";
      copyright = "(c) 2020, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/postgresql-syntax";
      url = "";
      synopsis = "PostgreSQL AST parsing and rendering";
      description = "Postgres syntax tree and related utils extracted from the \\\"hasql-th\\\" package.\nThe API is in a rather raw \\\"guts out\\\" state with most documentation lacking,\nbut the codebase is well tested.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."headed-megaparsec" or (errorHandler.buildDepError "headed-megaparsec"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-builder" or (errorHandler.buildDepError "text-builder"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "tasty-test" = {
          depends = [
            (hsPkgs."postgresql-syntax" or (errorHandler.buildDepError "postgresql-syntax"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "hedgehog-test" = {
          depends = [
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."postgresql-syntax" or (errorHandler.buildDepError "postgresql-syntax"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/postgresql-syntax-0.4.1.tar.gz";
      sha256 = "93cde438f4ca505450dc9c85d13c3e2868c5e268f25110c2e8c0b7bd9e9443d3";
      });
    }) // {
    package-description-override = "name: postgresql-syntax\nversion: 0.4.1\ncategory: Database, PostgreSQL, Parsing\nsynopsis: PostgreSQL AST parsing and rendering\ndescription:\n  Postgres syntax tree and related utils extracted from the \\\"hasql-th\\\" package.\n  The API is in a rather raw \\\"guts out\\\" state with most documentation lacking,\n  but the codebase is well tested.\nhomepage: https://github.com/nikita-volkov/postgresql-syntax\nbug-reports: https://github.com/nikita-volkov/postgresql-syntax/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2020, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/postgresql-syntax.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: ApplicativeDo, Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    PostgresqlSyntax.Ast\n    PostgresqlSyntax.KeywordSet\n    PostgresqlSyntax.Parsing\n    PostgresqlSyntax.Rendering\n    PostgresqlSyntax.Validation\n  other-modules:\n    PostgresqlSyntax.CharSet\n    PostgresqlSyntax.Extras.TextBuilder\n    PostgresqlSyntax.Extras.HeadedMegaparsec\n    PostgresqlSyntax.Extras.NonEmpty\n    PostgresqlSyntax.Predicate\n    PostgresqlSyntax.Prelude\n  build-depends:\n    base >=4.12 && <5,\n    bytestring >=0.10 && <0.12,\n    case-insensitive >=1.2.1 && <2,\n    hashable >=1.3.5 && <2,\n    headed-megaparsec >=0.2.0.1 && <0.3,\n    megaparsec >=9.2 && <10,\n    parser-combinators >=1.3 && <1.4,\n    text >=1 && <3,\n    text-builder >=0.6.6.3 && <0.7,\n    unordered-containers >=0.2.16 && <0.3\n\ntest-suite tasty-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tasty-test\n  main-is: Main.hs\n  default-extensions: ApplicativeDo, Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  build-depends:\n    postgresql-syntax,\n    QuickCheck >=2.10 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10 && <0.11,\n    tasty-quickcheck >=0.10 && <0.11\n\ntest-suite hedgehog-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: hedgehog-test\n  default-extensions: ApplicativeDo, Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  main-is: Main.hs\n  other-modules:\n    Main.Gen\n  build-depends:\n    hedgehog >=1.0.1 && <2,\n    postgresql-syntax,\n    rerebase >=1.6.1 && <2\n";
    }