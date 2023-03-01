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
      identifier = {
        name = "template-haskell-compat-v0208";
        version = "0.1.9.1";
        };
      license = "MIT";
      copyright = "(c) 2016, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/template-haskell-compat-v0208";
      url = "";
      synopsis = "A backwards compatibility layer for Template Haskell newer than 2.8";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/template-haskell-compat-v0208-0.1.9.1.tar.gz";
      sha256 = "a12a623f5f9b58af58e65b8020805fc01964562a5e4db973836fb14d14cd07fd";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: template-haskell-compat-v0208\nversion: 0.1.9.1\ncategory: TemplateHaskell, Compatibility\nsynopsis: A backwards compatibility layer for Template Haskell newer than 2.8\nhomepage: https://github.com/nikita-volkov/template-haskell-compat-v0208\nbug-reports: https://github.com/nikita-volkov/template-haskell-compat-v0208/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2016, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/template-haskell-compat-v0208.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    TemplateHaskell.Compat.V0208\n  other-modules:\n    TemplateHaskell.Compat.V0208.Prelude\n  build-depends:\n    base >=4.6 && <5,\n    template-haskell >=2.8 && <2.20\n";
    }