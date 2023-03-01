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
      identifier = { name = "hasql-pool"; version = "0.5.2.2"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/hasql-pool ";
      url = "";
      synopsis = "A pool of connections for Hasql";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."base-prelude" or (errorHandler.buildDepError "base-prelude"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base-prelude" or (errorHandler.buildDepError "base-prelude"))
            (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
            (hsPkgs."hasql-pool" or (errorHandler.buildDepError "hasql-pool"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hasql-pool-0.5.2.2.tar.gz";
      sha256 = "05e316836b932478408f052f77f657f3daeb1e5eeb34aca43aaf343fb78b286b";
      });
    }) // {
    package-description-override = "name:\n  hasql-pool\nversion:\n  0.5.2.2\ncategory:\n  Hasql, Database, PostgreSQL\nsynopsis:\n  A pool of connections for Hasql\nhomepage:\n  https://github.com/nikita-volkov/hasql-pool \nbug-reports:\n  https://github.com/nikita-volkov/hasql-pool/issues \nauthor:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:\n  (c) 2015, Nikita Volkov\nlicense:\n  MIT\nlicense-file:\n  LICENSE\nbuild-type:\n  Simple\ncabal-version:\n  >=1.10\n\n\nsource-repository head\n  type:\n    git\n  location:\n    git://github.com/nikita-volkov/hasql-pool.git\n\n\nlibrary\n  hs-source-dirs:\n    library\n  ghc-options:\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  other-modules:\n    Hasql.Pool.Prelude\n    Hasql.Pool.ResourcePool\n  exposed-modules:\n    Hasql.Pool\n  build-depends:\n    -- resources:\n    resource-pool >= 0.2 && < 0.3,\n    -- database:\n    hasql >= 1.3 && < 1.6,\n    -- data:\n    time >= 1.5 && < 2,\n    -- general:\n    base-prelude >= 1 && < 2\n\n\ntest-suite test\n  type:\n    exitcode-stdio-1.0\n  hs-source-dirs:\n    test\n  main-is:\n    Main.hs\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    base-prelude,\n    hasql,\n    hasql-pool,\n    hspec >= 2.6 && < 3\n";
    }