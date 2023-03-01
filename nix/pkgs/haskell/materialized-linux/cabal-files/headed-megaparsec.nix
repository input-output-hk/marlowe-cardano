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
      identifier = { name = "headed-megaparsec"; version = "0.2.1"; };
      license = "MIT";
      copyright = "(c) 2019, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/headed-megaparsec";
      url = "";
      synopsis = "More informative parser";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."selective" or (errorHandler.buildDepError "selective"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/headed-megaparsec-0.2.1.tar.gz";
      sha256 = "283048b701b4e560e522c9f1510a8465762a264a3daf1fc3f78744d2b6fc649e";
      });
    }) // {
    package-description-override = "name: headed-megaparsec\nversion: 0.2.1\ncategory: Parsers, Parsing, Megaparsec\nsynopsis: More informative parser\nhomepage: https://github.com/nikita-volkov/headed-megaparsec\nbug-reports: https://github.com/nikita-volkov/headed-megaparsec/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2019, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/headed-megaparsec.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: ApplicativeDo, Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    HeadedMegaparsec\n  other-modules:\n    HeadedMegaparsec.Megaparsec\n    HeadedMegaparsec.Prelude\n  build-depends:\n    base >=4.12 && <5,\n    case-insensitive >=1.2 && <2,\n    megaparsec >=9.2 && <10,\n    parser-combinators >=1.3 && <1.4,\n    selective >=0.5 && <0.6\n";
    }