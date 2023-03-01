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
      identifier = { name = "hasql-transaction"; version = "1.0.1.1"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/hasql-transaction";
      url = "";
      synopsis = "Composable abstraction over retryable transactions for Hasql";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-tree-builder" or (errorHandler.buildDepError "bytestring-tree-builder"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."contravariant-extras" or (errorHandler.buildDepError "contravariant-extras"))
          (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "conflicts-test" = {
          depends = [
            (hsPkgs."contravariant-extras" or (errorHandler.buildDepError "contravariant-extras"))
            (hsPkgs."hasql-transaction" or (errorHandler.buildDepError "hasql-transaction"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hasql-transaction-1.0.1.1.tar.gz";
      sha256 = "65083dc7474026eff8ef55200772e73a63adf8ed52b9b9c48b7aa0a9d33b532a";
      });
    }) // {
    package-description-override = "name: hasql-transaction\nversion: 1.0.1.1\ncategory: Hasql, Database, PostgreSQL\nsynopsis: Composable abstraction over retryable transactions for Hasql\nhomepage: https://github.com/nikita-volkov/hasql-transaction\nbug-reports: https://github.com/nikita-volkov/hasql-transaction/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2015, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/hasql-transaction.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    Hasql.Transaction\n    Hasql.Transaction.Sessions\n  other-modules:\n    Hasql.Transaction.Private.Prelude\n    Hasql.Transaction.Private.Model\n    Hasql.Transaction.Private.SQL\n    Hasql.Transaction.Private.Statements\n    Hasql.Transaction.Private.Sessions\n    Hasql.Transaction.Private.Transaction\n  build-depends:\n    base >=4.12 && <5,\n    bytestring >=0.10 && <0.12,\n    bytestring-tree-builder >=0.2.7.8 && <0.3,\n    contravariant >=1.3 && <2,\n    contravariant-extras >=0.3 && <0.4,\n    hasql >=1.4.5 && <1.6,\n    mtl >=2.2 && <3,\n    transformers >=0.5 && <0.7\n\ntest-suite conflicts-test\n  type: exitcode-stdio-1.0\n  hs-source-dirs: conflicts-test\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  main-is: Main.hs\n  other-modules:\n    Main.Statements\n    Main.Transactions\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n  build-depends:\n    contravariant-extras >=0.3 && <0.4,\n    hasql-transaction,\n    hasql,\n    async >=2.1 && <3,\n    rerebase >=1.11 && <2\n";
    }