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
      identifier = { name = "hasql-th"; version = "0.4.0.17"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/hasql-th";
      url = "";
      synopsis = "Template Haskell utilities for Hasql";
      description = "Extension-library for Hasql,\nbringing compile-time syntax checking,\ngreat simplification of declaration of statements and\nother TemplateHaskell-based utilities.\n\nFor details please see <https://github.com/nikita-volkov/hasql-th the readme>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
          (hsPkgs."postgresql-syntax" or (errorHandler.buildDepError "postgresql-syntax"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."template-haskell-compat-v0208" or (errorHandler.buildDepError "template-haskell-compat-v0208"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hasql-th-0.4.0.17.tar.gz";
      sha256 = "2c1e5fae0a8faaeeae5b6a36605d74a47cff3380e339a8c88cd37c45225299e8";
      });
    }) // {
    package-description-override = "name: hasql-th\nversion: 0.4.0.17\ncategory: Hasql, Database, PostgreSQL, Template Haskell\nsynopsis: Template Haskell utilities for Hasql\ndescription:\n  Extension-library for Hasql,\n  bringing compile-time syntax checking,\n  great simplification of declaration of statements and\n  other TemplateHaskell-based utilities.\n  .\n  For details please see <https://github.com/nikita-volkov/hasql-th the readme>.\nhomepage: https://github.com/nikita-volkov/hasql-th\nbug-reports: https://github.com/nikita-volkov/hasql-th/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2015, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/hasql-th.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: ApplicativeDo, Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    Hasql.TH\n  other-modules:\n    Hasql.TH.Construction.Exp\n    Hasql.TH.Extraction.ChildExprList\n    Hasql.TH.Extraction.Exp\n    Hasql.TH.Extraction.InputTypeList\n    Hasql.TH.Extraction.OutputTypeList\n    Hasql.TH.Extraction.PlaceholderTypeMap\n    Hasql.TH.Extraction.PrimitiveType\n    Hasql.TH.Prelude\n  build-depends:\n    base >=4.11 && <5,\n    bytestring >=0.10 && <0.12,\n    containers >=0.6 && <0.7,\n    contravariant >=1.5.2 && <2,\n    foldl >=1.4.5 && <2,\n    hasql >=1.4 && <1.7,\n    postgresql-syntax >=0.4.1 && <0.5,\n    template-haskell >=2.8 && <3,\n    template-haskell-compat-v0208 >=0.1.9 && <2,\n    text >=1 && <3,\n    uuid >=1.3 && <2,\n    vector >=0.12 && <0.14\n";
    }