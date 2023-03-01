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
      identifier = { name = "postgresql-binary"; version = "0.12.5"; };
      license = "MIT";
      copyright = "(c) 2014, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/postgresql-binary ";
      url = "";
      synopsis = "Encoders and decoders for the PostgreSQL's binary format";
      description = "An API for dealing with PostgreSQL's binary data format.\n\nIt can be used to implement performant bindings to Postgres.\nE.g., <http://hackage.haskell.org/package/hasql hasql>\nis based on this library.\n\nIt supports all Postgres versions starting from 8.3\nand is tested against 8.3, 9.3 and 9.5\nwith the @integer_datetimes@ setting off and on.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary-parser" or (errorHandler.buildDepError "binary-parser"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bytestring-strict-builder" or (errorHandler.buildDepError "bytestring-strict-builder"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."network-ip" or (errorHandler.buildDepError "network-ip"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "tasty" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."network-ip" or (errorHandler.buildDepError "network-ip"))
            (hsPkgs."postgresql-binary" or (errorHandler.buildDepError "postgresql-binary"))
            (hsPkgs."postgresql-libpq" or (errorHandler.buildDepError "postgresql-libpq"))
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
        "encoding" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."postgresql-binary" or (errorHandler.buildDepError "postgresql-binary"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        "decoding" = {
          depends = [
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."postgresql-binary" or (errorHandler.buildDepError "postgresql-binary"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/postgresql-binary-0.12.5.tar.gz";
      sha256 = "7227f2d31593d0bf07c6a6e4c3dfd5b9235587f812c188d78d0a30c2eb49cc3a";
      });
    }) // {
    package-description-override = "name:\n  postgresql-binary\nversion:\n  0.12.5\nsynopsis:\n  Encoders and decoders for the PostgreSQL's binary format\ndescription:\n  An API for dealing with PostgreSQL's binary data format.\n  .\n  It can be used to implement performant bindings to Postgres.\n  E.g., <http://hackage.haskell.org/package/hasql hasql>\n  is based on this library.\n  .\n  It supports all Postgres versions starting from 8.3 \n  and is tested against 8.3, 9.3 and 9.5\n  with the @integer_datetimes@ setting off and on.\ncategory:\n  PostgreSQL, Database, Codecs, Parsing\nhomepage:\n  https://github.com/nikita-volkov/postgresql-binary \nbug-reports:\n  https://github.com/nikita-volkov/postgresql-binary/issues \nauthor:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:\n  (c) 2014, Nikita Volkov\nlicense:\n  MIT\nlicense-file:\n  LICENSE\nbuild-type:\n  Simple\ncabal-version:\n  >=1.10\n\nsource-repository head\n  type:\n    git\n  location:\n    git://github.com/nikita-volkov/postgresql-binary.git\n\nlibrary\n  hs-source-dirs:\n    library\n  ghc-options:\n    -funbox-strict-fields\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  exposed-modules:\n    PostgreSQL.Binary.Decoding\n    PostgreSQL.Binary.Encoding\n    PostgreSQL.Binary.Data\n  other-modules:\n    PostgreSQL.Binary.Encoding.Builders\n    PostgreSQL.Binary.Prelude\n    PostgreSQL.Binary.Integral\n    PostgreSQL.Binary.Interval\n    PostgreSQL.Binary.Numeric\n    PostgreSQL.Binary.Time\n    PostgreSQL.Binary.Inet\n    PostgreSQL.Binary.BuilderPrim\n  build-depends:\n    aeson >=2 && <3,\n    base >=4.12 && <5,\n    binary-parser >=0.5.7 && <0.6,\n    bytestring >=0.10.4 && <0.12,\n    bytestring-strict-builder >=0.4.5.4 && <0.5,\n    containers >=0.5 && <0.7,\n    network-ip >=0.3 && <0.4,\n    scientific >=0.3 && <0.4,\n    text >=1.2 && <3,\n    time >=1.9 && <2,\n    transformers >=0.3 && <0.7,\n    unordered-containers ==0.2.*,\n    uuid ==1.3.*,\n    vector >=0.12 && <0.14\n\n-- This test-suite must be executed in a single-thread.\ntest-suite tasty\n  type:\n    exitcode-stdio-1.0\n  hs-source-dirs:\n    tasty\n  main-is:\n    Main.hs\n  other-modules:\n    Main.TextEncoder \n    Main.DB\n    Main.Apx\n    Main.IO\n    Main.Gens\n    Main.PTI\n    Main.Properties\n    Main.Prelude\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    aeson >=2 && <3,\n    network-ip >=0.2 && <1,\n    postgresql-binary,\n    postgresql-libpq ==0.9.*,\n    QuickCheck >=2.10 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase >=1.10.0.1 && <2,\n    tasty >=1.4 && <2,\n    tasty-hunit >=0.10 && <0.11,\n    tasty-quickcheck >=0.10 && <0.11\n\nbenchmark encoding\n  type: \n    exitcode-stdio-1.0\n  hs-source-dirs:\n    encoding\n  main-is:\n    Main.hs\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n    -funbox-strict-fields\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    criterion >=1.5.9 && <2,\n    postgresql-binary,\n    rerebase >=1.10.0.1 && <2\n\nbenchmark decoding\n  type: \n    exitcode-stdio-1.0\n  hs-source-dirs:\n    decoding\n  main-is:\n    Main.hs\n  ghc-options:\n    -O2\n    -threaded\n    \"-with-rtsopts=-N\"\n    -funbox-strict-fields\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    criterion >=1.5.9 && <2,\n    postgresql-binary,\n    rerebase >=1.10.0.1 && <2\n";
    }